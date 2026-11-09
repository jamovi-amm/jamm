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
        updateScaling(ui,this);
    },

    onChange_covariates: function(ui) {
        updateScaling(ui, this);
        updateSuppliers(ui,this);
        
    },

    onChange_modelSupplier: function(ui) {
       // Supplier refreshes during restoration must not edit saved options.
       if (this.workspace.initializing)
           return;
       fromSupplierToModelTerms(ui,this);
    },
    onUpdate_modelSupplier: function(ui) {
       console.log("modelUpated");
    },

    onChange_mediatorsSupplier: function(ui) {
       // Only genuine supplier edits should propagate to the regressions.
       if (this.workspace.initializing)
           return;
       log("mediatorsSupplier has changed");
       fromSupplierToMediatorsTerms(ui, this);
       fromMediatorsToModelTerms(ui, this);

    },

    onUpdate_mediatorsSupplier: function(ui) {
            log("mediatorsSupplier update");
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            // Match updateSuppliers so opening this control does not remove mediators.
            let mediatorsList = this.cloneArray(ui.mediators.value(), []);
            var variablesList = factorsList.concat(covariatesList).concat(mediatorsList);
            ui.mediatorsSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));

    },

    onChange_moderatorsSupplier: function(ui) {
          // Restoring controls must leave the saved moderator specification intact.
          if (this.workspace.initializing)
              return;
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
        // Ignore notifications emitted while restoring the saved controls.
        if (this.workspace.initializing || this.workspace.revertingModerators)
            return;
        console.log("moderators have changed");
        fromModeratorsToOthers(ui,this);
   },
  

    onEvent_mediatorToTerms: function(ui) {
      // Rebinding a saved term list is not an edit to its regression equation.
      if (this.workspace.initializing ||
          findChangesMulti("mediatorsTerms", this.cloneArray(ui.mediatorsTerms.value(), []), this, false).index < 0)
          return;
      log("mediators terms changed");
      
    fromMediatorsTerms(ui,this);
    fromMediatorsToModelTerms(ui,this);
    },
    
     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },


};



var initializeAll = function(ui, context) {
    // Seed change tracking before supplier updates can emit change events.
    var factors = context.cloneArray(ui.factors.value(), []);
    var covariates = context.cloneArray(ui.covs.value(), []);
    var mediators = context.cloneArray(ui.mediators.value(), []);
    var components = factors.concat(covariates).concat(mediators);
    context.workspace.modelSupplierList = context.cloneArray(components);
    context.workspace.mediatorsSupplierList = context.cloneArray(components);
    context.workspace.moderatorsSupplierList = factors.concat(covariates);
    context.workspace.mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(), []);
    context.workspace.moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(), []);
    context.workspace.impliedModelTerms = impliedModelTerms(ui, context);

    // Saved outcome terms are authoritative, even if defaults would differ.
    context.workspace.initializing = true;
    try {
       updateSuppliers(ui,context);
       labelize(ui.mediatorsTerms, mediators, "Mediator");
       labelize(ui.moderatorsTerms, mediators, "Mediator");
    }
    finally {
       context.workspace.initializing = false;
    }
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
     // Supplier initialization and repeated notifications are not model edits.
     if (diff.added.length === 0 && diff.removed.length === 0)
         return;
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
    log(light)
    // Store first so synchronous control notifications see the new baseline.
    storeComponent("mediatorsTerms",context.cloneArray(light),context);
    ui.mediatorsTerms.setValue(light);    
    labelize(ui.mediatorsTerms,mediators, "Mediator");  
};


var fromMediatorsTerms= function(ui,context) {

    var mediators = context.cloneArray(ui.mediators.value(), []);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var moderatorsTerms= context.cloneArray(ui.moderatorsTerms.value(),[]);

    for (var i = 0; i < mediators.length; i++)
        mediatorsTerms[i]=removeFromList(mediators[i],mediatorsTerms[i],context,1);
    
    var diff=findChangesMulti("mediatorsTerms",mediatorsTerms,context);
    if (diff.index>-1 && diff.index<mediatorsTerms.length) {
         moderatorsTerms[diff.index]=removeFromList(diff.changes[diff.index].removed,moderatorsTerms[diff.index],context,1) ;
         mediatorsTerms[diff.index]=removeFromList(diff.changes[diff.index].removed,mediatorsTerms[diff.index],context,1) ;
       }
        ui.mediatorsTerms.setValue(mediatorsTerms);
        ui.moderatorsTerms.setValue(moderatorsTerms);
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


var fromModeratorsToOthers = function(ui,context) {
    console.log("from moderators to others");
    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var modelTerms = context.cloneArray(ui.modelTerms.value(),[]);
    // Reserve at least one independent variable across all moderator blocks.
    // This is a global role check, not the old per-mediator predictor check.
    var predictors = context.cloneArray(ui.factors.value(), []).concat(
        context.cloneArray(ui.covs.value(), []));
    var selected = [];
    moderatorsTerms.forEach(function(block) {
        normalize(block).forEach(function(term) {
            selected = selected.concat(term);
        });
    });
    var pending = findChangesMulti("moderatorsTerms",moderatorsTerms,context,false);
    var hasAdditions = pending.changes.some(function(change) { return change.added.length > 0; });
    if (predictors.length > 0 && hasAdditions && predictors.every(function(predictor) {
        return selected.indexOf(predictor) > -1;
    })) {
        // Reject the transfer before it can add interactions to either model.
        // Preserve removals made in the same edit, and suppress rollback events.
        moderatorsTerms = moderatorsTerms.map(function(block, index) {
            return removeFromList(pending.changes[index].added, block, context, 0);
        });
        context.workspace.revertingModerators = true;
        try {
            ui.moderatorsTerms.setValue(moderatorsTerms);
        } finally {
            context.workspace.revertingModerators = false;
        }
    }
    // Save the baseline before updating controls that can emit change events.
    var diff = findChangesMulti("moderatorsTerms",moderatorsTerms,context,true);
         if (diff.index > -1) {
          // add interactions to mediators and full model terms if there are new moderators
           for (var i = 0; i < diff.changes[diff.index].added.length; i++) {
             var newTerm=diff.changes[diff.index].added[i];
         // add interactions to mediators terms if there are new moderators
             mediatorsTerms[diff.index]=combine(mediatorsTerms[diff.index],newTerm,context,0);
             mediatorsTerms[diff.index]=cleanInteractions(moderatorsTerms[diff.index],mediatorsTerms[diff.index],context);
          // add interactions to full model terms if there are new moderators
          // we should add interactions with the other IV and only the selected mediator
          var others=combine(mediatorsTerms[diff.index],newTerm,context,1);
          var int=newTerm[0];
          int.push(mediators[diff.index]);
          others.push(int);
          modelTerms=modelTerms.concat(others);

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

};

var  fromMediatorsToModelTerms = function(ui,context) {
     log("fromMediatorsToModelTerms");
     if (context.workspace.initializing)
         return;

     // Add only newly implied terms; preserve outcome-only terms and intentional omissions.
     var implied = impliedModelTerms(ui, context);
     // Supplier events can arrive before initialization creates this snapshot.
     // Default before cloning: jamovi's cloneArray cannot JSON-clone undefined.
     var previous = context.cloneArray(context.workspace.impliedModelTerms === undefined ? [] : context.workspace.impliedModelTerms, []);
     var added = removeFromList(previous, implied, context, 0);
     context.workspace.impliedModelTerms = context.cloneArray(implied);
     if (added.length > 0) {
         var modelTerms = context.cloneArray(ui.modelTerms.value(), []);
         ui.modelTerms.setValue(addToList(added, modelTerms, context));
     }
};

// Calculate defaults separately so they can be compared without replacing saved terms.
var impliedModelTerms = function(ui,context) {
     var mediators = context.cloneArray(ui.mediators.value(),[]);
     var mediatorsTerms = flatMulti(context.cloneArray(ui.mediatorsTerms.value(),[]),context);
     var modelTerms = [];
     modelTerms = addToList(mediatorsTerms,mediators,context);
     var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);
     var meds=[];
     for (var i = 0; i < mediators.length; i++) {
           var ameds=combine(moderatorsTerms[i],mediators[i],context,0);
           meds=meds.concat(ameds);
     }
    modelTerms=addToList(unique(meds),modelTerms,context);
    return modelTerms;

};

var updateScaling = function(ui,context) {
    log("updateScaling");
    var currentList = context.cloneArray(ui.scaling.value(), []);
    var variableList = context.cloneArray(ui.covs.value(), [])
    var mediatorsList = context.cloneArray(ui.mediators.value(), [])
    variableList=variableList.concat(mediatorsList)

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
            list3.push({ var: variableList[i], type: "simple" });
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

