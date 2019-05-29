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
               if (dim(aCosmos)===0)
                     aCosmos=[aCosmos]
                 if (FormatDef.term.isEqual(aCosmos,quantum)) {
                        light=true;
                        break;
                     }
              }
    return(light);
};

