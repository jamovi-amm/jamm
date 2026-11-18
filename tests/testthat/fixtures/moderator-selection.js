var copy = function(value) { return JSON.parse(JSON.stringify(value)); };
var FormatDef = { term: {
    isEqual: function(left, right) {
        return JSON.stringify(left.slice().sort()) === JSON.stringify(right.slice().sort());
    },
    contains: function(left, right) {
        return right.every(function(value) { return left.indexOf(value) >= 0; });
    }
}};
var control = function(value) {
    return {
        current: value,
        value: function() { return this.current; },
        setValue: function(value) { this.current = copy(value); }
    };
};
var context = {
    workspace: { mod_terms: [[]] },
    cloneArray: function(value, fallback) { return copy(value === undefined ? fallback : value); },
    clone: copy,
    getCombinations: function(values) { return values.map(function(value) { return [value]; }); }
};
var ui = {
    factors: control([]), covs: control(["predictor"]), mediators: control(["mediator"]),
    mod_terms: control([[["predictor"]]]),
    mediatorsTerms: control([[["predictor"]]]),
    modelTerms: control([["predictor"], ["mediator"]])
};
var check = function(condition, message) { if (!condition) throw new Error(message); };
var originalModels = JSON.stringify([ui.mediatorsTerms.value(), ui.modelTerms.value()]);
module.exports.onEvent_moderatorChanged.call(context, ui);
check(ui.mod_terms.value()[0].length === 0, "Sole predictor was accepted");
check(JSON.stringify([ui.mediatorsTerms.value(), ui.modelTerms.value()]) === originalModels,
    "Rejected transfer changed regression terms");

// Simulate the general update snapshot already containing the invalid selection.
ui.mod_terms.current = [[["predictor"]]];
context.workspace.mod_terms = copy(ui.mod_terms.current);
module.exports.onEvent_moderatorChanged.call(context, ui);
check(ui.mod_terms.value()[0].length === 0, "Refreshed snapshot bypassed validation");

// A factor counts as a remaining independent variable.
ui.factors.current = ["group"];
ui.mod_terms.current = [[["predictor"]]];
module.exports.onEvent_moderatorChanged.call(context, ui);
check(ui.mod_terms.value()[0].length === 1, "Valid moderator was rejected");
check(ui.modelTerms.value().some(function(term) {
    return term.indexOf("predictor") >= 0 && term.indexOf("mediator") >= 0;
}), "Valid moderator did not add its interaction");

// Independent variables must remain available across all mediator blocks.
ui.mediators.current = ["mediator", "secondMediator"];
ui.mod_terms.current = [[["predictor"]], [["group"]]];
module.exports.onEvent_moderatorChanged.call(context, ui);
check(ui.mod_terms.value()[1].length === 0, "All predictors became moderators");
var checksPassed = true;
