var DEBUG=true;

const events = {
    update: function(ui) {
      console.log("general update");
      initializeAll(ui, this);

    },

    onChange_factors: function(ui) {
        updateSuppliers(ui,this);
    },

    onChange_mediators: function(ui) {
      log("mediators have changed");
        updateMediatorsBlock(ui,this);
        updateSuppliers(ui,this);

    },

    onChange_covariates: function(ui) {
        updateSuppliers(ui,this);
    },

    onChange_modelSupplier: function(ui) {
       log("modelSupplier has changed");
       var modelSupplierList = this.cloneArray(this.itemsToValues(ui.modelSupplier.value()),[]);
       var diff = findChanges("modelSupplierList",modelSupplierList,this);
       if (diff.hasChanged>0)
            updateModelTerms(diff, ui, this);
       
    },
    onChange_mediatorsSupplier: function(ui) {
      
       log("mediatorsSupplier has changed");
       var mediatorsSupplierList = this.cloneArray(this.itemsToValues(ui.mediatorsSupplier.value()),[]);
       var diff = findChanges("mediatorsSupplierList",mediatorsSupplierList,this);
       if (diff.hasChanged>0)
           updateMediatorsTerms(diff,ui, this);
    },

    onChange_moderatorsSupplier: function(ui) {
          updateModeratorsTerms(ui, this);
    },

    onChange_modelTerms: function(ui) {
//        filterModelTerms(ui, this);
//        updateSimpleSupplier(ui, this);

    },

    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plotHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepPlots, false, values, FormatDef.variable);
    },
    
        onChange_simpleSupplier: function(ui) {
        let values = this.itemsToValues(ui.simpleSupplier.value());
        this.checkValue(ui.simpleVariable, false, values, FormatDef.variable);
        this.checkValue(ui.simpleModerator, false, values, FormatDef.variable);
        this.checkValue(ui.simple3way, false, values, FormatDef.variable);
    },


  onEvent_moderatorChanged: function(ui) {
          console.log("moderators change");

      var moderatorsTerms = this.cloneArray(ui.moderatorsTerms.value(),[]);
      var mediatorsTerms = this.cloneArray(ui.mediatorsTerms.value(),[]);
      var mediators = this.cloneArray(ui.mediators.value(),[]);

      var diff = this.findChanges("moderatorsTerms",moderatorsTerms,true,FormatDef.term);

    var inter=true;
    var termsList= mediatorsTerms;
    var test1= false;
    var test2= false;

    var aList=[];
    var aMod = [];
    for (var i = 0; i < mediators.length; i++) {
        test1 = !(moderatorsTerms[i]===undefined || moderatorsTerms[i].length===0);
        test2 = !(mediatorsTerms[i]===undefined || mediatorsTerms[i].length===0);
        if ( test1 & test2) {
          aMod=this.cloneArray(moderatorsTerms[i]);
          for (var j = 0; j < aMod.length; j++) {
            aList = combineOne(mediatorsTerms[i],aMod[j],this);            
          }
          var iList = getInteractions(aMod,this);
          for (var j = 0; j < aList.length; j++) {
            for (var k = 0; k < iList.length; k++) {
              if (inTerm(iList[k],aList[j],this)) {
               aList.splice(j , 1);
               j -= 1;
              }
              }
            }
         termsList[i] = aList;
        }
    }
    ui.mediatorsTerms.setValue(termsList);
    },
  

    onEvent_mediatorToTerms: function(ui) {
      console.log("mediators terms changed");
      
      var mediators = this.cloneArray(ui.mediators.value(),[]);
      var termsList = this.cloneArray(ui.mediatorsTerms.value(),[]);

      var modelList = this.cloneArray(ui.modelTerms.value(),[]);
      var diff = this.findChanges("mediatorsTerms",termsList,true,FormatDef.term);
     //  we travers the mediators terms list and, for each mediator, remove itself from the IV
     for (var i = 0; i < mediators.length; i++) {
       for (var j = 0; j < termsList[i].length; j++) {
         var ind=termsList[i][j].indexOf(mediators[i]);
         if (ind>-1) {
             termsList[i].splice(j,1);
         } else
          // if the term is an interaction, we add it to the full model terms
            if (termsList[i][j].length>1) 
                  modelList.push(this.cloneArray(termsList[i][j]));

       }
    }
    // if an interaction was remove, we remove it also from the full model

    if (diff.removed.length>0) 
        for (var i = 0; i < diff.removed.length; i++) 
             for (var j = 0; j < diff.removed[i].length; j++) {
                if(diff.removed[i][j].length>1)
                  modelList=removeFromList(diff.removed[i][j],modelList,this);
        }
    
    ui.mediatorsTerms.setValue(termsList);
    ui.modelTerms.setValue(unique(modelList));
    
    },
    
     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },


};



var initializeAll = function(ui, context) {
    
    updateSuppliers(ui,context);
    updateMediatorsBlock(ui,context);
    var modelSupplierList = context.cloneArray(context.itemsToValues(ui.modelSupplier.value(), []));
    context.workspace.modelSupplierList=modelSupplierList;
    var mediatorsSupplierList = context.cloneArray(context.itemsToValues(ui.mediatorsSupplier.value(), []));
    context.workspace.mediatorsSupplierList=mediatorsSupplierList;
    /// fix the mediatots terms 
    var mediators= context.cloneArray(ui.mediators.value(), []);
    var mediatorsTerms= context.cloneArray(ui.mediatorsTerms.value(), []);
    context.workspace.mediatorsTerms=mediatorsTerms;

    /// fix the moderators terms 
    var moderatorsTerms= context.cloneArray(ui.moderatorsTerms.value(), []);
    if (mediators.length===0)
        ui.moderatorsTerms.setValue([]); 
    else {
      updateModeratorsTerms(ui,context);
    }
    moderatorsTerms= context.cloneArray(ui.moderatorsTerms.value(), []);
    context.workspace.moderatorsTerms=moderatorsTerms;
    

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

var updateMediatorsBlock = function(ui,context) {

    var mediators = context.cloneArray(ui.mediators.value(), []);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    var moderatorsTerms = context.cloneArray(ui.moderatorsTerms.value(),[]);

    var newMedTerms = [];
    var newModTerms = [];

    if (mediators.length===0) {
      newMedTerms=[];
      newModTerms=[];
    } else { 
        for (var i = 0; i < mediators.length ; i++) {
            newMedTerms[i] = mediatorsTerms[i] === undefined ? [] : mediatorsTerms[i] ;
            newMedTerms[i] = moderatorsTerms[i] === undefined ? [] : moderatorsTerms[i] ;
        }
     }
     ui.mediatorsTerms.setValue(newMedTerms);
     labelize(ui.mediatorsTerms,mediators, "Dependent");  
     ui.moderatorsTerms.setValue(newModTerms);
     labelize(ui.moderatorsTerms,mediators, "Mediator");  

};

var updateMediatorsTerms= function(diff,ui,context) {

    log("updateMediatorsTerms");

    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var mediatorsTerms = context.cloneArray(ui.mediatorsTerms.value(),[]);
    if (termInList(diff.added,mediators,context)===false)
         log("not in mediators");
    else
        log("not found");
    


};


var updateModeratorsTerms= function(ui,context) {
    console.log("update moderators");
    var mediators = context.cloneArray(ui.mediators.value(),[]);
    var m_termsList = context.cloneArray(ui.moderatorsTerms.value(),[]);
    
    var termsList= new Array(mediators.length);
    var aList = [];
    for (var i = 0; i < mediators.length; i++) {
      if (m_termsList[i]!==undefined) {
            aList=context.cloneArray(m_termsList[i]);
      } else
          aList = [];
     termsList[i] = new Array(aList.length);
     for (var j = 0; j < aList.length; j++) {
      termsList[i][j] = aList[j];
     }
    }
    ui.moderatorsTerms.setValue(termsList);
    labelize(ui.moderatorsTerms,mediators, "Mediator");
    
    
};

var updateModelTerms= function(diff,ui,context) {
    
    log("updateModelTerms");
    var modelTerms = context.cloneArray(ui.modelTerms.value(), []);
        modelTerms = addToList(diff.added,modelTerms,context);
        modelTerms = removeFromList(diff.removed,modelTerms,context);
        ui.modelTerms.setValue(modelTerms);
};


// helper functions

var addToList = function(quantum, cosmos, context) {


    var cosmos = context.cloneArray(cosmos);
    var dimc = dim(cosmos);
    var dimq = dim(quantum);

    if (dimc<3) 
        for (var i = 0; i < quantum.length; i++) {
          if (dimq==1)
              cosmos.push([quantum[i]]);
          if (dimq==2)
              cosmos.push(quantum[i]);
          }
    return(unique(cosmos));
};


var alignList = function(allowed, aList, context) {

    if (allowed.length===0)
            return([]);
    var keep=0;
    for (var j = 0; j < aList.length; j++) {
              var aTerm = context.cloneArray(aList[j]);
              keep=0;
              for (var i = 0; i < allowed.length; i++) {
                  var aAllowed=context.clone(allowed[i]);

                     if (context.listContains(aTerm,aAllowed,FormatDef.term)===true)
                        keep += 1;
              }
              if (aTerm.length>keep) {
                        aList.splice(j, 1);
                        j -= 1;
                     }
              }
        
    return(aList);
};


var removeFromList = function(quantum, cosmos, context) {

    var cosmos = context.cloneArray(cosmos);
    var dimc = dim(cosmos);
    var dimq = dim(quantum);

    // standard lists, dim<3 //
    if (dimc < 3) 
       if (quantum.length>0)
          for (var j = 0; j < cosmos.length; j++) {
               var aCosmos = context.cloneArray(cosmos[j]);
                 if (FormatDef.term.isEqual(aCosmos,quantum)) {
                        cosmos.splice(j, 1);
                        j -= 1;
                     }
              }
      
      
    return(cosmos);
};


var termInList = function(quantum, cosmos, context) {

    var cosmos = context.cloneArray(cosmos);
    var dimc = dim(cosmos);
    var dimq = dim(quantum);
    var light = false;
    // standard lists, dim<3 
    if (dimc < 3) 
       if (quantum.length>0)
          for (var j = 0; j < cosmos.length; j++) {
               var aCosmos = context.cloneArray(cosmos[j]);
                 if (FormatDef.term.isEqual(aCosmos,quantum)) {
                        result=true;
                        break;
                     }
              }
    return(light);
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
var keysFromLabels = function(widget) {
     var keysList=[];
     widget.applyToItems(0, (item, index) => {
           var value=item.controls[0].getPropertyValue("label");
            keysList[index]=value;
        });
      return(keysList);
};


var labelsFromKeys = function(widget,labels, prefix) {

     var keysList = keysFromLabels(widget);
     for(var i = 0, l = labels.length; i < l; ++i) {
        labels[i] = prefix + " = " +labels[i];
    }
    
     keysList = unique(keysList.concat(labels));

     return(keysList);

};

var labelize = function(widget, labels, prefix) {
     widget.applyToItems(0, (item, index) => {
           item.controls[0].setPropertyValue("label",prefix +" = "+labels[index]);
        });
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

var getInteractions = function(aList,context) {
  
  var iList = context.getCombinations(aList);
  for (var i = 0; i < iList.length; i++ )
          if (iList[i].length===1) {
              iList.splice(i, 1);
               i -= 1;
          }
 return(iList);
  
};

var inTerm = function(smallTerm,largeTerm, context) {
    var value = false;
    var found = 0;  
    for (var i = 0; i < smallTerm.length ; i++)
             if (context.listContains(largeTerm,smallTerm[i],FormatDef.term))
                 found ++;
    if (found===smallTerm.length)
        value=true;
    return(value);
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

var findChanges= function(id,aList,context) {
  
  var adim=dim(aList);
  if (adim===1) 
     return(context.findChanges(id,aList,true,FormatDef.term));

  var original=context.workspace[id];
  if (original===undefined) 
            original=[];
  console.log("find");
  console.log(original);
  console.log("end");
  
};

var log=function(obj) {
    if (DEBUG)
      console.log(obj);
};

module.exports = events;

