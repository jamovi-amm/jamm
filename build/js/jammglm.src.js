
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"dep","title":"Dependent Variable","type":"Variable","default":null,"suggested":["continuous","ordinal"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable from `data`, variable must be numeric\n"}},{"name":"mediators","title":"Mediators","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the mediators from `data`"}},{"name":"factors","title":"Factors","type":"Variables","suggested":["nominal","ordinal"],"permitted":["factor"],"default":null,"description":{"R":"a vector of strings naming the fixed factors from `data`"}},{"name":"covs","title":"Covariates","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the covariates from `data`"}},{"name":"modelTerms","title":"Model Terms","type":"Terms","default":null,"description":{"R":"a list of character vectors describing fixed effects terms\n"}},{"name":"ciType","title":"Confidence Intervals","type":"List","options":[{"name":"standard","title":"Standard"},{"name":"bca","title":"Bootstrap (BC)"},{"name":"perc","title":"Bootstrap (Percent)"},{"name":"norm","title":"Bootstrap (Normal)"},{"name":"none","title":"None"}],"default":"standard","description":{"R":"Choose the confidence interval type\n"}},{"name":"ciWidth","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the parameter estimates\n"}},{"name":"bootN","title":"Bootstrap Rep.","type":"Number","min":50,"default":1000,"description":{"R":"number of bootstrap samples for estimating confidence intervals\n"}},{"name":"contrasts","title":"Factors Coding","type":"Array","items":"(factors)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["deviation","simple","dummy","difference","helmert","repeated","polynomial"],"default":"deviation"}]},"description":{"R":"a list of lists specifying the factor and type of contrast to use, one of `'deviation'`, `'simple'`, `'difference'`, `'helmert'`, `'repeated'` or `'polynomial'`\n"}},{"name":"showRealNames","title":"Names in estimates table","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), provide raw names of the contrasts variables\n"}},{"name":"showContrastCode","title":"Contrast Coefficients tables","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide contrast coefficients tables\n"}},{"name":"bogus","type":"Bool","default":false,"description":{"R":"`a bogus option to define a label without visible children`\n"}},{"name":"simpleScale","title":"Covariates conditioning","type":"List","options":[{"name":"mean_sd","title":"Mean ±  SD"},{"name":"percent","title":"Percentiles 50 ± offset"}],"default":"mean_sd","description":{"R":"`'mean_sd'` (default), `'custom'` , or `'custom_percent'`. Use to condition the covariates (if any)\n"}},{"name":"cvalue","type":"Number","default":1,"description":{"R":"offset value for conditioning\n"}},{"name":"percvalue","type":"Number","default":25,"min":5,"max":50,"description":{"R":"offset value for conditioning\n"}},{"name":"simpleScaleLabels","title":"Moderators labeling","type":"List","options":[{"name":"labels","title":"Labels"},{"name":"values","title":"Values"},{"name":"values_labels","title":"Values + Labels"}],"default":"labels"},{"name":"scaling","title":"Covariates Scaling","type":"Array","items":"(covs)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["centered","standardized","none"],"default":"centered"}]},"description":{"R":"a list of lists specifying the covariates scaling, one of `'centered to the mean'`, `'standardized'`, or `'none'`. `'none'` leaves the variable as it is\n"}},{"name":"tableOptions","title":"Display","type":"NMXList","options":[{"name":"component","title":"IE Components"},{"name":"beta","title":"β"},{"name":"regression","title":"Individual regressions"}],"default":["beta","component","regression"]},{"name":"pathOptions","type":"NMXList","options":[{"name":"suggested","title":"Suggested paths"}],"default":["suggested"]},{"name":"mediatorsTerms","title":"Models for mediators","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the models for with the mediators as dependent variables.          \n"}},{"name":"moderatorsTerms","title":"Mediated effects","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the the IV which moderatorate each mediated effect.          \n"}}];

const view = View.extend({
    jus: "2.0",

    events: [

	],

	update: require('./jamm.events').update,

	loaded: require('./jamm.events').loaded

});

view.layout = ui.extend({

    label: "GLM Mediation Model",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			name: "variablesupplier",
			suggested: ["continuous","nominal","ordinal"],
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "dep",
							maxItemCount: 1,
							isTarget: true,
							itemDropBehaviour: "overwrite"
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "mediators",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./jamm.events').onChange_mediators }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "factors",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./jamm.events').onChange_factors }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "covs",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./jamm.events').onChange_covariates }
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Mediators Models",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Supplier,
							name: "mediatorsSupplier",
							format: FormatDef.term,
							persistentItems: true,
							stretchFactor: 1,
							events: [
								{ onEvent: 'update', execute: require('./jamm.events').onUpdate_mediatorsSupplier },
								{ execute: require('./jamm.events').onChange_mediatorsSupplier }
							],
							controls: [
								{
									type: DefaultControls.TargetLayoutBox,
									transferAction: "interactions",
									controls: [
										{
											type: DefaultControls.ListBox,
											name: "mediatorsTerms",
											height: "large",
											events: [
												{ onEvent: 'listItemAdded', execute: require('./jamm.events').onEvent_nothing },
												{ onEvent: 'listItemRemoved', execute: require('./jamm.events').onEvent_nothing }
											],
											selectable: true,
											templateName: "linreg-block-template",
											template:
											{
												type: DefaultControls.LayoutBox,
												margin: "normal",
												targetArea: true,
												controls: [
													{
														type: DefaultControls.Label,
														label: "empty",
														name: "blockName",
														stretchFactor: 1,
														margin: "normal"
													},
													{
														type: DefaultControls.ListBox,
														enable: "(mediators)",
														name: "blockList",
														isTarget: true,
														valueFilter: "unique",
														height: "auto",
														ghostText: "drag variables here",
														events: [
															{ execute: require('./jamm.events').onEvent_mediatorToTerms }
														],
														template:
														{
															type: DefaultControls.TermLabel
														}														
													}
												]
											}											
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Full Model",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					name: "modelSupplier",
					label: "Components",
					persistentItems: true,
					stretchFactor: 1,
					format: FormatDef.term,
					events: [
						{ onEvent: 'update', execute: require('./jamm.events').onUpdate_modelSupplier },
						{ execute: require('./jamm.events').onChange_modelSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							transferAction: "interactions",
							controls: [
								{
									type: DefaultControls.ListBox,
									name: "modelTerms",
									valueFilter: "unique",
									isTarget: true,
									itemDropBehaviour: "emptyspace",
									events: [
										{ execute: require('./jamm.events').onChange_modelTerms }
									],
									template:
									{
										type: DefaultControls.TermLabel
									}									
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Moderators",
			name: "moderatorsBox",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Supplier,
							name: "moderatorsSupplier",
							format: FormatDef.term,
							persistentItems: true,
							stretchFactor: 1,
							events: [
								{ onEvent: 'update', execute: require('./jamm.events').onUpdate_moderatorsSupplier },
								{ execute: require('./jamm.events').onChange_moderatorsSupplier }
							],
							controls: [
								{
									type: DefaultControls.TargetLayoutBox,
									transferAction: "interactions",
									controls: [
										{
											type: DefaultControls.ListBox,
											name: "moderatorsTerms",
											height: "large",
											events: [
												{ onEvent: 'listItemAdded', execute: require('./jamm.events').onEvent_nothing },
												{ onEvent: 'listItemRemoved', execute: require('./jamm.events').onEvent_nothing }
											],
											selectable: true,
											templateName: "linreg-block-template",
											template:
											{
												type: DefaultControls.LayoutBox,
												margin: "normal",
												targetArea: true,
												controls: [
													{
														type: DefaultControls.Label,
														label: "empty",
														name: "modblockName",
														stretchFactor: 1,
														margin: "normal"
													},
													{
														type: DefaultControls.ListBox,
														enable: "(mediators)",
														name: "blockList",
														isTarget: true,
														valueFilter: "unique",
														height: "auto",
														ghostText: "drag variables here",
														events: [
															{ execute: require('./jamm.events').onEvent_moderatorChanged }
														],
														template:
														{
															type: DefaultControls.TermLabel
														}														
													}
												]
											}											
										}
									]
								}
							]
						}
					]
				},
				{
					type: DefaultControls.Label,
					name: "modeNote",
					margin: "small",
					label: "There are not enough independent variables in the mediator model to define a moderator",
					controls: [
						{
							type: DefaultControls.CheckBox,
							name: "bogus"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Factors Coding",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					name: "contrasts",
					stretchFactor: 1,
					showColumnHeaders: false,
					columns: [
						{
							name: "var",
							label: null,
							selectable: false,
							stretchFactor: 1,
							maxWidth: 300,
							template:
							{
								type: DefaultControls.VariableLabel
							}							
						},
						{
							name: "type",
							label: null,
							selectable: false,
							stretchFactor: 0.5,
							template:
							{
								type: DefaultControls.ComboBox
							}							
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					name: "showRealNames"
				},
				{
					type: DefaultControls.CheckBox,
					name: "showContrastCode"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Covariates Scaling",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					name: "scaling",
					stretchFactor: 1,
					showColumnHeaders: false,
					columns: [
						{
							name: "var",
							label: null,
							selectable: false,
							stretchFactor: 1,
							maxWidth: 300,
							template:
							{
								type: DefaultControls.VariableLabel
							}							
						},
						{
							name: "type",
							label: null,
							selectable: false,
							stretchFactor: 0.5,
							template:
							{
								type: DefaultControls.ComboBox
							}							
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							label: "Covariates conditioning",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									margin: "large",
									style: "list",
									controls: [
										{
											type: DefaultControls.RadioButton,
											name: "simpleScale_mean_sd",
											optionName: "simpleScale",
											optionPart: "mean_sd",
											controls: [
												{
													type: DefaultControls.TextBox,
													name: "cvalue",
													format: FormatDef.number
												}
											]
										},
										{
											name: "simpleScale_percent",
											type: DefaultControls.RadioButton,
											optionName: "simpleScale",
											optionPart: "percent",
											controls: [
												{
													type: DefaultControls.TextBox,
													name: "percvalue",
													label: null,
													suffix: "%",
													format: FormatDef.number,
													enable: "(simpleScale_percent)"
												}
											]
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							label: "Covariates labeling",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									margin: "large",
									controls: [
										{
											type: DefaultControls.RadioButton,
											name: "simpleScaleLabels_labels",
											optionName: "simpleScaleLabels",
											optionPart: "labels"
										},
										{
											type: DefaultControls.RadioButton,
											name: "simpleScaleLabels_numbers",
											optionName: "simpleScaleLabels",
											optionPart: "values"
										},
										{
											type: DefaultControls.RadioButton,
											name: "simpleScaleLabels_numbers_labels",
											optionName: "simpleScaleLabels",
											optionPart: "values_labels"
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Mediation options",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							label: "Confidence Intervals",
							margin: "large",
							style: "list",
							controls: [
								{
									type: DefaultControls.RadioButton,
									name: "ciType_standard",
									optionName: "ciType",
									optionPart: "standard"
								},
								{
									type: DefaultControls.RadioButton,
									name: "ciType_bootperc",
									optionName: "ciType",
									optionPart: "perc"
								},
								{
									type: DefaultControls.RadioButton,
									name: "ciType_bootbca",
									optionName: "ciType",
									optionPart: "bca"
								},
								{
									type: DefaultControls.RadioButton,
									name: "ciType_bootnorm",
									optionName: "ciType",
									optionPart: "norm"
								},
								{
									type: DefaultControls.RadioButton,
									name: "ciType_none",
									optionName: "ciType",
									optionPart: "none"
								},
								{
									type: DefaultControls.TextBox,
									name: "ciWidth",
									label: "Interval",
									suffix: "%",
									format: FormatDef.number
								},
								{
									type: DefaultControls.TextBox,
									name: "bootN",
									format: FormatDef.number
								}
							]
						},
						{
							type: DefaultControls.Label,
							label: "Display in tables",
							margin: "large",
							style: "list",
							controls: [
								{
									name: "tableOptions_component",
									type: DefaultControls.CheckBox,
									optionPart: "component",
									optionName: "tableOptions"
								},
								{
									name: "tableOptions_beta",
									type: DefaultControls.CheckBox,
									optionName: "tableOptions",
									optionPart: "beta"
								},
								{
									name: "tableOptions_regression",
									type: DefaultControls.CheckBox,
									optionName: "tableOptions",
									optionPart: "regression"
								}
							]
						},
						{
							type: DefaultControls.Label,
							label: "Path model",
							margin: "large",
							style: "list",
							controls: [
								{
									name: "pathOptions_suggested",
									type: DefaultControls.CheckBox,
									optionPart: "suggested",
									optionName: "pathOptions"
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
