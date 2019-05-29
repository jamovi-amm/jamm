const DEBUG=true;

const events = {
    update: function(ui) {
      console.log("general update");
      initializeAll(ui, this);
    },

     loaded: function(ui) {
        let $contents = ui.view.$el;
     },
        
    onChange_factors: function(ui) {
        updateSuppliers(ui,this);
         updateContrasts(ui,this);
    },

    onChange_mediators: function(ui) {
        updateSuppliers(ui,this);
    },

    onChange_covariates: function(ui) {
        updateScaling(ui, this);
        updateSuppliers(ui,this);
        
    },

    onChange_modelSupplier: function(ui) {
       fromSupplierToModelTerms(ui,this);
    },
    onUpdate_modelSupplier: function(ui) {
       console.log("modelUpated");
    },

    onChange_mediatorsSupplier: function(ui) {
       log("mediatorsSupplier has changed");
       fromSupplierToMediatorsTerms(ui, this);
       fromMediatorsToModelTerms(ui, this);

    },

    onUpdate_mediatorsSupplier: function(ui) {
            log("mediatorsSupplier update");
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.mediatorsSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));

    },

    onChange_moderatorsSupplier: function(ui) {
          updateModeratorsTerms(ui, this);
          fromSupplierToModeratorsTerms(ui,this);
    },

    onUpdate_moderatorsSupplier: function(ui) {
            log("moderatorsSupplier update");
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.moderatorsSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));
      
    },


    onChange_modelTerms: function(ui) {
//        filterModelTerms(ui, this);
//        updateSimpleSupplier(ui, this);

    },

    onChange_plotsSupplier: function(ui) {
    },
    
    onChange_simpleSupplier: function(ui) {
    },

    onEvent_moderatorChanged: function(ui) {
        console.log("moderators have changed");
        fromModeratorsToOthers(ui,this);
   },
  

    onEvent_mediatorToTerms: function(ui) {
      log("mediators terms changed");
      
    unflashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
    fromMediatorsTerms(ui,this);
    fromMediatorsToModelTerms(ui,this);
    },
    
     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },


};



var initializeAll = function(ui, context) {
    
    updateSuppliers(ui,context);
//    updateMediatorsTerms(ui, context);
//    updateModeratorsTerms(ui, context);
 
    var modelSupplierList = context.cloneArray(context.itemsToValues(ui.modelSupplier.value(), []));
    context.workspace.modelSupplierList=modelSupplierList;
    var mediatorsSupplierList = context.cloneArray(context.itemsToValues(ui.mediatorsSupplier.value(), []));
    context.workspace.mediatorsSupplierList=mediatorsSupplierList;

    /// fix the mediatots terms 
    var mediators= context.cloneArray(ui.mediators.value(), []);

    var mediatorsTerms= context.cloneArray(ui.mediatorsTerms.value(), []);
    context.workspace.mediatorsTerms=mediatorsTerms;

    var moderatorsTerms= context.cloneArray(ui.moderatorsTerms.value(), []);
    context.workspace.moderatorsTerms=moderatorsTerms;
    
    // this is for hiding the labels used for warnings 
       ui.bogus.$el[0].remove();
       ui.modeNote.$el[0].style.visibility="hidden";
       ui.modeNote.$el[0].style.color="red";
    
};




var updateSuppliers= function(ui,context) {

    var factorsList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var indList = factorsList.concat(covariatesList);
    var mediatorsList = context.cloneArray(ui.mediators.value(), []);
    var allList = factorsList.concat(covariatesList).concat(mediatorsList);
    
    ui.modelSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    ui.mediatorsSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    ui.moderatorsSupplier.setValue(context.valuesToItems(indList, FormatDef.variable));

};


var fromSupplierToMediatorsTerms= function(ui,context) {

    log("fromSupplierToMediatorsTerms");

     var mediatorsSupplierList = context.cloneArray(context.itemsToValues(ui.mediatorsSupplier.value()),[]);
     var diff = context.findChanges("mediatorsSupplierList",mediatorsSupplierList,context);
     var mediators = context.cloneArray(ui.mediators.value(),[]);
     var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
     var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);

     var factors =  context.cloneArray(ui.factors.value(),[]);
     var covs =  context.cloneArray(ui.covs.value(),[]);
     var inds = factors.concat(covs);
     var light = new Array(mediators.length);

     // we make sure that there are enough arrays in the array list, each for each mediator
     // check for moderators as well     
     // if a mediator terms are empty, we put in all independent variables, otherwise we put in
     // the added variable in the supplier. In all cases, we combine the to be added variables
     // with the moderators, if any
     
     for (var i = 0; i < mediators.length; i++) {
         var aList = mediatorsTerms[i] === undefined ? [] : mediatorsTerms[i] ;
         var add = aList.length === 0 ? inds : removeFromList(mediators,diff.added,context);
         var add = addToList(add,aList,context);
         light[i] = combine(moderatorsTerms[i],add,context,0);
         light[i] = cleanInteractions(moderatorsTerms[i],light[i],context);
     }
     
     // we remove the independent variables removed from the ui, if any
    if (diff.removed.length>0)
        light = removeFromMultiList(diff.removed,light,context,1);
    
    ui.mediatorsTerms.setValue(light);    
    labelize(ui.mediatorsTerms,mediators, "Mediator");  
    storeComponent("mediatorsTerms",light,context);
};


var fromMediatorsTerms= function(ui,context) {

    var mediators = context.cloneArray(ui.mediators.value(), []);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var moderatorsTerms= context.cloneArray(ui.moderatorsTerms.value(),[]);

    for (var i = 0; i < mediators.length; i++)
        mediatorsTerms[i]=removeFromList(mediators[i],mediatorsTerms[i],context,1);
    
    var diff=findChangesMulti("mediatorsTerms",mediatorsTerms,context);
    if (diff.index>-1) {
         moderatorsTerms[diff.index]=removeFromList(diff.changes[diff.index].removed,moderatorsTerms[diff.index],context,1) ;
         mediatorsTerms[diff.index]=removeFromList(diff.changes[diff.index].removed,mediatorsTerms[diff.index],context,1) ;
       }

        ui.mediatorsTerms.setValue(mediatorsTerms);
        ui.moderatorsTerms.setValue(moderatorsTerms);
        isRoomForModerators(ui,context)
};
var fromSupplierToModeratorsTerms= function(ui,context) {

    log("fromSupplierToModeratorsTerms");

     var moderatorsSupplierList = context.cloneArray(context.itemsToValues(ui.moderatorsSupplier.value()),[]);
     var diff = context.findChanges("moderatorsSupplierList",moderatorsSupplierList,context);
     var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
     var  light = removeFromMultiList(diff.removed,moderatorsTerms,context,1);
     ui.moderatorsTerms.setValue(light);    

};

var fromSupplierToModelTerms= function(ui,context) {

    log("fromSupplierToModelTerms");

     var modelSupplierList = context.cloneArray(context.itemsToValues(ui.modelSupplier.value()),[]);
     var modelTerms= context.cloneArray(ui.modelTerms.value(),[]);
     var diff = context.findChanges("modelSupplierList",modelSupplierList,context);
     var  light = removeFromList(diff.removed,modelTerms,context,1);
     ui.modelTerms.setValue(light);    

};



var updateModeratorsTerms= function(ui,context) {
    console.log("update moderators");

    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
    var light = new Array(mediators.length);

     for (var i = 0; i < mediators.length; i++) {
         light[i]= moderatorsTerms[i] === undefined ? [] : moderatorsTerms[i] ;
     }
    
    ui.moderatorsTerms.setValue(light);
    labelize(ui.moderatorsTerms,mediators, "Mediator");

};


var isRoomForModerators = function(ui,context) {
    console.log("checkRoomForModerators");
    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var moderators = flatMulti(moderatorsTerms,context);
     

     var diff = findChangesMulti("moderatorsTerms",moderatorsTerms,context,false);

    // here we check if there are enough predictors in the mediator model to 
    // define one moderator
    var noroom = false;
    for (var j = 0; j < mediators.length; j++)  { 
       var meds = mediatorsTerms[j];
           meds = removeFromList(moderators,meds,context);
           if (meds.length===0) {
              flashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
              noroom = true;
      }
    }
      // if there's no room (noroom==true) for an additional moderator, we remove it 
      // from the new list and left the old list saved in the workspace (see findChangesMulti(...,false) )
      var removeAdded= diff.index>-1 && noroom===true;
          if (removeAdded) {
            var moderatorsTest= context.cloneArray(moderatorsTerms);
            moderatorsTest[diff.index] = removeFromList(diff.changes[diff.index].added,moderatorsTest[diff.index],context);
            ui.moderatorsTerms.setValue(moderatorsTest);
      }
   return !removeAdded;
};


var fromModeratorsToOthers = function(ui,context) {
    console.log("from moderators to others");
    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var modelTerms = context.cloneArray(ui.modelTerms.value(),[]);
    var moderators = flatMulti(moderatorsTerms,context);
     

     var diff = findChangesMulti("moderatorsTerms",moderatorsTerms,context,false);

    // here we check if there are enough predictors in the mediator model to 
    // define one moderator
    var noroom = false;
    for (var j = 0; j < mediators.length; j++)  { 
       var meds = mediatorsTerms[j];
           var medstest = removeFromList(moderators,meds,context);
           if (medstest.length===0) {
              flashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
              noroom = true;
      }
    }
      // if there's no room (noroom==true) for an additional moderator, we remove it 
      // from the new list and left the old list saved in the workspace (see findChangesMulti(...,false) )
      var removeAdded= diff.index>-1 && noroom===true;
          if (removeAdded) {
            var moderatorsTest= context.cloneArray(moderatorsTerms);
            moderatorsTest[diff.index] = removeFromList(diff.changes[diff.index].added,moderatorsTest[diff.index],context);
            ui.moderatorsTerms.setValue(moderatorsTest);
      }

      // if nothing happens, we spread the moderators. However, we need
      // to check diff again, bacause we did not save moderatorsTerms before
      // to avoid a loop that would unflash the warning no matter what.
      // d
      if (removeAdded===false) {

         var diff2 = findChangesMulti("moderatorsTerms",moderatorsTerms,context,true);
         if (diff2.index > -1) {
           unflashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
          // add interactions to mediators and full model terms if there are new moderators
           for (var i = 0; i < diff.changes[diff.index].added.length; i++) {
             var newTerm=diff.changes[diff.index].added[i];
         // add interactions to mediators terms if there are new moderators
             mediatorsTerms[diff.index]=combine(mediatorsTerms[diff.index],newTerm,context,0);
             mediatorsTerms[diff.index]=cleanInteractions(moderatorsTerms[diff.index],mediatorsTerms[diff.index],context);
          // add interactions to full model terms if there are new moderators
           modelTerms=combine(modelTerms,newTerm,context,0);
           modelTerms=cleanInteractions(moderators,modelTerms,context);
      }
      // remove interactions from mediators and full model terms if moderators are removed
      for (var i = 0; i < diff.changes[diff.index].removed.length; i++)  {
           var oldValue = diff.changes[diff.index].removed[i];
           // for mediators terms
           mediatorsTerms[diff.index]=removeFromList(oldValue,mediatorsTerms[diff.index],context,2);
           // for full model terms
           modelTerms=removeFromList(oldValue,modelTerms,context,2);
      }

          ui.mediatorsTerms.setValue(mediatorsTerms);
          ui.modelTerms.setValue(modelTerms);
      
      }

  } 

};

var  fromMediatorsToModelTerms = function(ui,context) {
    
     log("fromMediatorsToModelTerms");
     var mediators = context.cloneArray(ui.mediators.value(),[]);
     var mediatorsTerms = flatMulti(context.cloneArray(ui.mediatorsTerms.value(),[]),context);
     var modelTerms = [];
     modelTerms = addToList(mediatorsTerms,mediators,context);
     var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
     var meds=[];
     for (var i = 0; i < mediators.length; i++) {
           var ameds=combine(moderatorsTerms[i],modelTerms,context,0);
           meds=meds.concat(ameds);
     }
    modelTerms=addToList(unique(meds),modelTerms,context);
    ui.modelTerms.setValue(modelTerms);

};

var updateScaling = function(ui,context) {
    log("updateScaling");
    var currentList = context.cloneArray(ui.scaling.value(), []);
    var variableList = context.cloneArray(ui.covs.value(), [])
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "centered" });
        else
            list3.push(found);
    }

    ui.scaling.setValue(list3);
};

var updateContrasts = function(ui, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);
    var variableList = context.cloneArray(ui.factors.value(), [])
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "deviation" });
        else
            list3.push(found);
    }

    ui.contrasts.setValue(list3);
};



// helper functions

// add an item or a list of items (quantum) to a list (cosmos).
// it tries to understand what kind of input it has, but it is not clear
// when it works
var addToList = function(quantum, cosmos, context) {
  
    cosmos = normalize(context.cloneArray(cosmos));
    quantum = normalize(context.cloneArray(quantum));
    
    for (var i = 0; i < quantum.length; i++) {
          if (dim(quantum[i])===0)
              cosmos.push([quantum[i]]);
          if (dim(quantum[i])===1)
              cosmos.push(quantum[i]);
          }
    return unique(cosmos);
};


var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};



// remove a list or a item from list
// order=0 remove only if term and target term are equal
// order>0 remove if term length>=order 
// for instance, order=1 remove any matching interaction with terms, keeps main effects
// order=2 remove from 3-way interaction on (keep up to 2-way interactions)

var removeFromList = function(quantum, cosmos, context, order = 1) {

     cosmos=normalize(cosmos);
     quantum=normalize(quantum);
     if (cosmos===undefined)
        return([]);
     var cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};




var unique=function(arr) {
    var u = {}, a = [];
    for(var i = 0, l = arr.length; i < l; ++i){
        var prop=ssort(JSON.stringify(arr[i]));
        if(!u.hasOwnProperty(prop) && arr[i].length>0) {
            a.push(arr[i]);
            u[prop] = 1;
        }
    }
    return a;
};

var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
}

var labelize = function(widget, labels, prefix) {
     widget.applyToItems(0, (item, index) => {
           item.controls[0].setPropertyValue("label",prefix +" = "+labels[index]);
        });
};


var flatMulti = function(cosmos,context) {
  var light = []
  for (var i=0 ; i < cosmos.length; i++) {
    light=addToList(light,cosmos[i],context);
  }
  return unique(light);
};

var combineOne = function(values, mod, context) {
        if (mod===undefined)
            return(values);
        var list = unique(values.concat([mod]));
        for (var i = 0; i < values.length; i++) {
            var newValue = context.clone(mod);
            var value = values[i];
            if (context.listContains(value,newValue[0],FormatDef.term)===false)
                   if (FormatDef.term.isEqual(newValue,value)===false) {
                      if (Array.isArray(value)) 
                          newValue = newValue.concat(value);
                      else
                         newValue.push(value);   
            list.push(newValue);
            }
        }
        return unique(list);
};

var getInteractions = function(aList,context,order=2) {
  
  var iList = context.getCombinations(aList);
  for (var i = 0; i < iList.length; i++ )
          if (iList[i].length===1 || iList[i].length>order) {
              iList.splice(i, 1);
               i -= 1;
          }
 return(iList);
  
};

var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos]
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue
        }
        return cosmos;
}

// get interaction between two lists
// order==0 include main effects
// 1  only interaction


var combine = function(cosmos1, cosmos2 , context, order=2) {
        if (cosmos1===cosmos2)
               return;
        if (cosmos1===undefined)
               return order===0 ? cosmos2 : [] ;
        if (cosmos2===undefined)
               return order===0 ? cosmos1 : [] ;
        cosmos1 = normalize(cosmos1)
        cosmos2 = normalize(cosmos2)        

        var light=[];
        for (var i = 0; i < cosmos1.length; i++) {
            var aValue1 = context.clone(cosmos1[i]);
            for (var j = 0 ; j < cosmos2.length; j ++) {
            var aValue2 = context.clone(cosmos2[j]);
            var join = aValue1.concat(aValue2);
            if (join.length===unique(join).length)
                   light.push(join);
              }
            }

        if (order===0)
              light = cosmos1.concat(cosmos2).concat(light);
        return unique(light);
};


var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};

var findChangesMulti= function(id,cosmos,context,save=true) {

  var old = context.workspace[id];
 if (old===undefined)
        old=[];
      
  var light = [];
  var len = Math.max(cosmos.length,old.length);
  var changeIndex = -1;
  for (var i = 0; i < len; i++) {
    var photon=[];
    photon.added = removeFromList(old[i], cosmos[i], context, 0);
    photon.removed = removeFromList(cosmos[i],old[i], context, 0);
    if  (photon.added.length > 0 || photon.removed.length > 0) 
          changeIndex = i
    light[i] = photon;
}
  light = { changes: light, index: changeIndex };
  if (save)
        storeComponent(id,cosmos,context);
  return(light);
};

var storeComponent = function(id,cosmos,context) {
                    context.workspace[id]=cosmos;
};

var log=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var dlog=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var flashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="#ffcccc";
        widget.controls[i].controls[1].$el[0].style.borderColor="red";
        if (note!==false)
           note.$el[0].style.visibility="visible";
        

};

var unflashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="white";
        widget.controls[i].controls[1].$el[0].style.borderColor="rgb(46,138,199)";
        if (note!==false)
           note.$el[0].style.visibility="hidden";
};

var   cleanInteractions= function(quantum,cosmos,context){
  if (quantum===undefined)
    return(cosmos);
    
  var deny= context.getCombinations(quantum)  
  for (var i=0; i < deny.length; i++) {
    if (deny[i].length==1) {
      deny.splice(i, 1);
      i -= 1;
    }
  }
  cosmos=context.cloneArray(cosmos)
  for (var j=0; j< cosmos.length; j++) {
    for (var i=0; i < deny.length; i++) {
      if (FormatDef.term.contains(cosmos[j],deny[i])) {
        cosmos.splice(j,1);
        j -= 1;
        break;
      }
  }
}
 return cosmos;
  
};


module.exports = events;

