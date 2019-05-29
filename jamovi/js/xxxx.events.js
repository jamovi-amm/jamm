
const events = {
    update: function(ui) {
      console.log("general update");
      initializeAll(ui, this);

    },

    onChange_factors: function(ui) {
        var diffs=updateFactors(ui, this);
        updateModelTerms(ui,diffs,this);
    },

    onChange_mediators: function(ui) {

    },

    onChange_covariates: function(ui) {

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

    onEvent_test_listItemsAdded: function(ui) {
      console.log("something itemsadded");
    },

    onEvent_test_listItemsChanged: function(ui) {
      console.log("something itemchanged");
    },
     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    }

    
};

var prepareMediatorsBlocks = function(ui, context) {
    console.log("do nothing");

    };


var initializeAll = function(ui, context) {
    
    var mediatorsList = context.cloneArray(ui.mediators.value(), []);
    var factorsList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    console.log("inizializing");
    console.log(context.workspace);

};

var updateFactors = function(ui, context) {

    var factorsList = context.cloneArray(ui.factors.value(), []);
    var diff = context.findChanges("factorsList", factorsList, true, FormatDef.variable);
    console.log(diff);
    return(diff);

};

var updateCovariates = function(ui, context) {

    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var diff = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    return(diff);

};


var updateMediators = function(ui, context) {

    var mediatorsList = context.cloneArray(ui.covs.value(), []);
    var diff = context.findChanges("mediatorsList", mediatorsList, true, FormatDef.variable);
    return(diff);

};

var updateMediatorsBlocks = function(ui, context) {

    var umList = context.cloneArray(ui.mediators.value(), []);
    var ufList = context.cloneArray(ui.factors.value(), []);
    var ucList = context.cloneArray(ui.covs.value(), []);
    var independentList = ufList.concat(ucList);
    var allList = independentList.concat(umList);
    
     ui.mediatorsSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));

    var mediatorsTerms = ui.mediatorsTerms;

    var diff = context.findChanges("umList", umList, true, FormatDef.variable);
    console.log(diff);
    for (var i = 0; i < diff.removed.length; i++) {
        var label="Dependent = " + diff.removed[i];
        mediatorsTerms.applyToItems(0, (item, index) => {
           var value=item.controls[0].getPropertyValue("label");
           if (label==value) {
                ui.mediatorsTerms.disposeOfRows(index);
        }
     });
    }
       
    
    
    for (var i = 0; i < diff.added.length; i++) {
        console.log("Im adding");
        var label="Dependent = " + diff.added[i];
        var newindex= mediatorsTerms.contentRowCount();
        console.log("new item "+newindex);
        if (newindex!=1)
                 mediatorsTerms.insertRow(newindex,1);       
           mediatorsTerms.updateDisplayRow(newindex, "");
           mediatorsTerms.applyToItems(newindex,1, function(item) {
           item.controls[0].setPropertyValue("label",label);
           });
     
     }
     
/*
     var TermsList = context.cloneArray(ui.mediatorsTerms.value(), []);
     TermsList[TermsList.length]=independentList;
     ui.mediatorsTerms.setValue(TermsList);
*/
};


var updateModelTerms = function(diffs, ui, context) {
  

    var added = diffs.added;
    console.log(added);
    return();    
    ui.modelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.simpleSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
 
    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);

    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var termsChanged = false;

    for (var i = 0; i < combinedDiff.removed.length; i++) {
        for (var j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], combinedDiff.removed[i])) {
                termsList.splice(j, 1);
                termsChanged = true;
                j -= 1;
            }
        }
    }


    for (var a = 0; a < diff.added.length; a++) {
        let item = diff.added[a];
        var listLength = termsList.length;
        for (var j = 0; j < listLength; j++) {
            var newTerm = context.clone(termsList[j]);
            if (containsCovariate(newTerm, covariatesList) === false) {
                if (context.listContains(newTerm, item, FormatDef.variable) === false) {
                    newTerm.push(item)
                    if (context.listContains(termsList, newTerm , FormatDef.term) === false) {
                        termsList.push(newTerm);
                        termsChanged = true;
                    }
                }
            }
        }
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    for (var a = 0; a < diff2.added.length; a++) {
        let item = diff2.added[a];
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    if (termsChanged) 
        ui.modelTerms.setValue(termsList);

    updateContrasts(ui, variableList, context);
    updateScaling(ui, covariatesList, context);
};



var calcModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var mediatorsList = context.cloneArray(ui.mediators.value(), []);
        covariatesList = covariatesList.concat(mediatorsList);
    var combinedList = variableList.concat(covariatesList);
    
    
    ui.modelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.simpleSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
 
    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);

    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var termsChanged = false;

    for (var i = 0; i < combinedDiff.removed.length; i++) {
        for (var j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], combinedDiff.removed[i])) {
                termsList.splice(j, 1);
                termsChanged = true;
                j -= 1;
            }
        }
    }


    for (var a = 0; a < diff.added.length; a++) {
        let item = diff.added[a];
        var listLength = termsList.length;
        for (var j = 0; j < listLength; j++) {
            var newTerm = context.clone(termsList[j]);
            if (containsCovariate(newTerm, covariatesList) === false) {
                if (context.listContains(newTerm, item, FormatDef.variable) === false) {
                    newTerm.push(item)
                    if (context.listContains(termsList, newTerm , FormatDef.term) === false) {
                        termsList.push(newTerm);
                        termsChanged = true;
                    }
                }
            }
        }
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    for (var a = 0; a < diff2.added.length; a++) {
        let item = diff2.added[a];
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    if (termsChanged) 
        ui.modelTerms.setValue(termsList);

    updateContrasts(ui, variableList, context);
    updateScaling(ui, covariatesList, context);
};


var calcMediatorsTerms = function(ui, context) {
  
    var mvariableList = context.cloneArray(ui.factors.value(), []);
    if (mvariableList.length===0)
               return(true);
               
    var mcovariatesList = context.cloneArray(ui.covs.value(), []);
    var mcombinedList = mvariableList.concat(mcovariatesList);

    console.log("updating the mediators");
    
    let varsDiff = context.findChanges("mcombinedList", mcombinedList, true, FormatDef.variable);


    let termsList = context.cloneArray(ui.mediatorsTerms.value(), []);

    let blocksNumber=ui.mediatorsTerms._rowCount-1;
    var termsChanged = false;
    console.log("blocknumber "+blocksNumber);
    console.log("removed ");
    console.log(varsDiff.removed);

    for (var i = 0; i < varsDiff.removed.length; i++) {
        for (var j = 0; j < blocksNumber; j++) {
            if (termsList[j] === undefined || termsList[j].length == 0)
                             termsList[j] = [];
              console.log("termlist"+j);
              console.log(termsList[j]);

            for (var k = 0; k < termsList[j].length; k++) {
              console.log("termlist"+j+" "+k);

              console.log(termsList[j][k]);
              console.log("removed"+i);

              console.log(varsDiff.removed[i]);
                if (FormatDef.term.contains(termsList[j][k], varsDiff.removed[i])) {
                    termsList[j].splice(k, 1);
                    termsChanged = true;
                    k -= 1;
                }
            }
        }
    }
       console.log("added");
       console.log(varsDiff.added);
       for (var a = 0; a < varsDiff.added.length; a++) {
            let item = varsDiff.added[a];
            for (var j = 0; j < blocksNumber; j++) {
               console.log(termsList.length+" "+j);
               if (termsList.length< (j+1))
                          termsList[j]=[];
                          console.log([item]);
                          console.log(termsList[j]);
                    if (context.listContains(termsList[j], [item] , FormatDef.term) === false) {
                    termsList[j].push([item]);
                    termsChanged = true;
             }
          }
       }
 
    console.log(termsChanged);
    console.log(termsList);
    if (termsChanged)
        ui.mediatorsTerms.setValue(termsList);
        
};


var updateSimpleSupplier = function(ui, context) {
        var termsList = context.cloneArray(ui.modelTerms.value(), []);
        var varList=[];
        for (var j = 0; j < termsList.length; j++) {
            var newTerm=context.clone(termsList[j]);
            if (newTerm.length==1) {
                  varList.push(newTerm[0]); // was varList.push(newTerm);
            }
        }
        varList=context.valuesToItems(varList, FormatDef.variable);
        ui.simpleSupplier.setValue(varList);
        ui.plotsSupplier.setValue(varList);

    };




var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var diff = context.findChanges("termsList", termsList, true, FormatDef.term);

    var changed = false;
    if (diff.removed.length > 0) {
        var itemsRemoved = false;
        for (var i = 0; i < diff.removed.length; i++) {
            var item = diff.removed[i];
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], item)) {
                    termsList.splice(j, 1);
                    j -= 1;
                    itemsRemoved = true;
                }
            }
        }

        if (itemsRemoved)
            changed = true;
    }

    if (context.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(termsList);
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);

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

var updateScaling = function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.scaling.value(), []);

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

var containsCovariate = function(value, covariates) {
    for (var i = 0; i < covariates.length; i++) {
        if (FormatDef.term.contains(value, covariates[i]))
            return true;
    }

    return false;
};







module.exports = events;

