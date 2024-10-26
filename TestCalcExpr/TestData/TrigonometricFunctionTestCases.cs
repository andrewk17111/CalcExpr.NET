﻿using CalcExpr.Expressions;
using CalcExpr.Expressions.Terminals;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr.TestData;

public static partial class TestCases
{
    public readonly static FunctionTestCase[] TrigonometricFunctions =
    [
        new FunctionTestCase("sin", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(0.8414709848078965) },
            { TestValues.NEGATIVE_ONE, new Number(-0.8414709848078965) },
            { TestValues.PI, new Number(1.2246467991473532E-16) },
            { TestValues.NEGATIVE_PI, new Number(-1.2246467991473532E-16) },
            { TestValues.PI_OVER_FOUR, new Number(0.7071067811865476) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.7071067811865476) },
            { TestValues.PI_OVER_TWO, TestValues.ONE },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.NEGATIVE_ONE },
            { TestValues.TAU, new Number(-2.4492935982947064E-16) },
            { TestValues.NEGATIVE_TAU, new Number(2.4492935982947064E-16) },
            { TestValues.THREE_PI_OVER_TWO, TestValues.NEGATIVE_ONE },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.ONE },
        }),
        new FunctionTestCase("sinh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.INFINITY },
            { TestValues.NEGATIVE_INFINITY, TestValues.NEGATIVE_INFINITY },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(1.1752011936438014) },
            { TestValues.NEGATIVE_ONE, new Number(-1.1752011936438014) },
            { TestValues.PI, new Number(11.548739357257746) },
            { TestValues.NEGATIVE_PI, new Number(-11.548739357257746) },
            { TestValues.PI_OVER_FOUR, new Number(0.8686709614860095) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.8686709614860095) },
            { TestValues.PI_OVER_TWO, new Number(2.3012989023072947) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-2.3012989023072947) },
            { TestValues.TAU, new Number(267.74489404101644) },
            { TestValues.NEGATIVE_TAU, new Number(-267.74489404101644) },
        }),
        new FunctionTestCase("cos", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, new Number(0.5403023058681398) },
            { TestValues.NEGATIVE_ONE, new Number(0.5403023058681398) },
            { TestValues.PI, TestValues.NEGATIVE_ONE },
            { TestValues.NEGATIVE_PI, TestValues.NEGATIVE_ONE },
            { TestValues.PI_OVER_FOUR, new Number(0.7071067811865476) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(0.7071067811865476) },
            { TestValues.PI_OVER_TWO, new Number(6.123233995736766E-17) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(6.123233995736766E-17) },
            { TestValues.TAU, TestValues.ONE },
            { TestValues.NEGATIVE_TAU, TestValues.ONE },
            { TestValues.THREE_PI_OVER_TWO, new Number(-1.8369701987210297E-16) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-1.8369701987210297E-16) },
        }),
        new FunctionTestCase("cosh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.INFINITY },
            { TestValues.NEGATIVE_INFINITY, TestValues.INFINITY },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, new Number(1.5430806348152437) },
            { TestValues.NEGATIVE_ONE, new Number(1.5430806348152437) },
            { TestValues.PI, new Number(11.591953275521519) },
            { TestValues.NEGATIVE_PI, new Number(11.591953275521519) },
            { TestValues.PI_OVER_FOUR, new Number(1.324609089252006) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(1.324609089252006) },
            { TestValues.PI_OVER_TWO, new Number(2.5091784786580567) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(2.5091784786580567) },
            { TestValues.TAU, new Number(267.7467614837482) },
            { TestValues.NEGATIVE_TAU, new Number(267.7467614837482) },
        }),
        new FunctionTestCase("tan", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(1.5574077246549) },
            { TestValues.NEGATIVE_ONE, new Number(-1.5574077246549) },
            { TestValues.PI, new Number(-1.2246467991473532E-16) },
            { TestValues.NEGATIVE_PI, new Number(1.2246467991473532E-16) },
            { TestValues.PI_OVER_FOUR, TestValues.ONE },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.NEGATIVE_ONE },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, new Number(-2.4492935982947064E-16) },
            { TestValues.NEGATIVE_TAU, new Number(2.4492935982947064E-16) },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("tanh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ONE },
            { TestValues.NEGATIVE_INFINITY, TestValues.NEGATIVE_ONE },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(0.761594155955765) },
            { TestValues.NEGATIVE_ONE, new Number(-0.761594155955765) },
            { TestValues.PI, new Number(0.99627207622075) },
            { TestValues.NEGATIVE_PI, new Number(-0.99627207622075) },
            { TestValues.PI_OVER_FOUR, new Number(0.655794202632672) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.655794202632672) },
            { TestValues.PI_OVER_TWO, new Number(0.917152335667274) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-0.917152335667274) },
            { TestValues.TAU, new Number(0.999993025339611) },
            { TestValues.NEGATIVE_TAU, new Number(-0.999993025339611) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.999838613988633) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-0.999838613988633) },
        }),
        new FunctionTestCase("arcsin", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, TestValues.PI_OVER_TWO },
            { TestValues.NEGATIVE_ONE, new Number(-1.5707963267949) },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, new Number(0.903339110766513) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.903339110766513) },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("arcsinh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.INFINITY },
            { TestValues.NEGATIVE_INFINITY, TestValues.NEGATIVE_INFINITY },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(0.881373587019543) },
            { TestValues.NEGATIVE_ONE, new Number(-0.881373587019543) },
            { TestValues.PI, new Number(1.86229574331085) },
            { TestValues.NEGATIVE_PI, new Number(-1.86229574331085) },
            { TestValues.PI_OVER_FOUR, new Number(0.72122548872678) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.72122548872678) },
            { TestValues.PI_OVER_TWO, new Number(1.23340311751122) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-1.23340311751122) },
            { TestValues.TAU, new Number(2.53729750137336) },
            { TestValues.NEGATIVE_TAU, new Number(-2.53729750137336) },
            { TestValues.THREE_PI_OVER_TWO, new Number(2.25441459299271) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-2.25441459299271) },
        }),
        new FunctionTestCase("arccos", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.PI_OVER_TWO },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.PI },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, new Number(0.667457216028384) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(2.47413543756141) },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("arccosh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.INFINITY },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.UNDEFINED },
            { TestValues.PI, new Number(1.81152627246085) },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.PI_OVER_TWO, new Number(1.02322747854755) },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, new Number(2.52463065993347) },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, new Number(2.23188925305808) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("arctan", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.PI_OVER_TWO },
            { TestValues.NEGATIVE_INFINITY, TestValues.NEGATIVE_PI_OVER_TWO },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, new Number(0.785398163397448) },
            { TestValues.NEGATIVE_ONE, new Number(-0.785398163397448) },
            { TestValues.PI, new Number(1.26262725567891) },
            { TestValues.NEGATIVE_PI, new Number(-1.26262725567891) },
            { TestValues.PI_OVER_FOUR, new Number(0.665773750028354) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-0.665773750028354) },
            { TestValues.PI_OVER_TWO, new Number(1.00388482185389) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-1.00388482185389) },
            { TestValues.TAU, new Number(1.41296513650674) },
            { TestValues.NEGATIVE_TAU, new Number(-1.41296513650674) },
            { TestValues.THREE_PI_OVER_TWO, new Number(1.36169168297116) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-1.36169168297116) },
        }),
        new FunctionTestCase("arctanh", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, TestValues.INFINITY },
            { TestValues.NEGATIVE_ONE, TestValues.NEGATIVE_INFINITY },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, new Number(1.05930617082324) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-1.05930617082324) },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("csc", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, new Number(1.18839510577812) },
            { TestValues.NEGATIVE_ONE, new Number(-1.18839510577812) },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, new Number(1.4142135623731) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-1.4142135623731) },
            { TestValues.PI_OVER_TWO, TestValues.ONE },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.NEGATIVE_ONE },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, TestValues.NEGATIVE_ONE },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.ONE },
        }),
        new FunctionTestCase("csch", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, new Number(0.850918128239322) },
            { TestValues.NEGATIVE_ONE, new Number(-0.850918128239322) },
            { TestValues.PI, new Number(0.0865895375300469) },
            { TestValues.NEGATIVE_PI, new Number(-0.0865895375300469) },
            { TestValues.PI_OVER_FOUR, new Number(1.15118387092085) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-1.15118387092085) },
            { TestValues.PI_OVER_TWO, new Number(0.434537208094696) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-0.434537208094696) },
            { TestValues.TAU, new Number(0.00373489848828567) },
            { TestValues.NEGATIVE_TAU, new Number(-0.00373489848828567) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.0179680320537773) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-0.0179680320537773) },
        }),
        new FunctionTestCase("sec", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, new Number(1.85081571768093) },
            { TestValues.NEGATIVE_ONE, new Number(1.85081571768093) },
            { TestValues.PI, TestValues.NEGATIVE_ONE },
            { TestValues.NEGATIVE_PI, TestValues.NEGATIVE_ONE },
            { TestValues.PI_OVER_FOUR, new Number(1.41421356237309) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(1.41421356237309) },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, TestValues.ONE },
            { TestValues.NEGATIVE_TAU, TestValues.ONE },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("sech", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, new Number(0.648054273663885) },
            { TestValues.NEGATIVE_ONE, new Number(0.648054273663885) },
            { TestValues.PI, new Number(0.0862667383340544) },
            { TestValues.NEGATIVE_PI, new Number(0.0862667383340544) },
            { TestValues.PI_OVER_FOUR, new Number(0.754939708714131) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(0.754939708714131) },
            { TestValues.PI_OVER_TWO, new Number(0.398536815338387) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(0.398536815338387) },
            { TestValues.TAU, new Number(0.00373487243863713) },
            { TestValues.NEGATIVE_TAU, new Number(0.00373487243863713) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.017965132264752) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(0.017965132264752) },
        }),
        new FunctionTestCase("cot", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, new Number(0.642092615934331) },
            { TestValues.NEGATIVE_ONE, new Number(-0.642092615934331) },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, TestValues.ONE },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.NEGATIVE_ONE },
            { TestValues.PI_OVER_TWO, new Number(6.123233995736766E-17) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-6.123233995736766E-17) },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, new Number(1.83772268236293E-16) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-1.8369701987210297E-16) },
        }),
        new FunctionTestCase("coth", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ONE },
            { TestValues.NEGATIVE_INFINITY, TestValues.NEGATIVE_ONE },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, new Number(1.31303528549933) },
            { TestValues.NEGATIVE_ONE, new Number(-1.31303528549933) },
            { TestValues.PI, new Number(1.00374187319732) },
            { TestValues.NEGATIVE_PI, new Number(-1.00374187319732) },
            { TestValues.PI_OVER_FOUR, new Number(1.52486861882206) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-1.52486861882206) },
            { TestValues.PI_OVER_TWO, new Number(1.09033141072737) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-1.09033141072737) },
            { TestValues.TAU, new Number(1.00000697470904) },
            { TestValues.NEGATIVE_TAU, new Number(-1.00000697470904) },
            { TestValues.THREE_PI_OVER_TWO, new Number(1.00016141206102) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-1.00016141206102) },
        }),
        new FunctionTestCase("arccsc", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, TestValues.PI_OVER_TWO },
            { TestValues.NEGATIVE_ONE, new Number(-1.5707963267949) },
            { TestValues.PI, new Number(0.32394611) },
            { TestValues.NEGATIVE_PI, new Number(-0.32394611) },
            { TestValues.PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.PI_OVER_TWO, new Number(0.69010709) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-0.69010709) },
            { TestValues.TAU, new Number(0.15983463) },
            { TestValues.NEGATIVE_TAU, new Number(-0.15983463) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.21383243) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-0.21383243) },
        }),
        new FunctionTestCase("arccsch", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, new Number(0.88137359) },
            { TestValues.NEGATIVE_ONE, new Number(-0.88137359) },
            { TestValues.PI, new Number(0.31316588) },
            { TestValues.NEGATIVE_PI, new Number(-0.31316588) },
            { TestValues.PI_OVER_FOUR, new Number(1.0620288) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(-1.0620288) },
            { TestValues.PI_OVER_TWO, new Number(0.59997148) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-0.59997148) },
            { TestValues.TAU, new Number(0.15849058) },
            { TestValues.NEGATIVE_TAU, new Number(-0.15849058) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.21064536) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-0.21064536) },
        }),
        new FunctionTestCase("arcsec", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.PI_OVER_TWO },
            { TestValues.NEGATIVE_INFINITY, TestValues.PI_OVER_TWO },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.PI },
            { TestValues.PI, new Number(1.2468502) },
            { TestValues.NEGATIVE_PI, new Number(1.8947424) },
            { TestValues.PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.PI_OVER_TWO, new Number(0.88068924) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(2.2609034) },
            { TestValues.TAU, new Number(1.4109617) },
            { TestValues.NEGATIVE_TAU, new Number(1.730631) },
            { TestValues.THREE_PI_OVER_TWO, new Number(1.3569639) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(1.7846288) },
        }),
        new FunctionTestCase("arcsech", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_INFINITY, TestValues.UNDEFINED },
            { TestValues.ZERO, TestValues.INFINITY },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.UNDEFINED },
            { TestValues.PI, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI, TestValues.UNDEFINED },
            { TestValues.PI_OVER_FOUR, new Number(0.72336752) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.TAU, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_TAU, TestValues.UNDEFINED },
            { TestValues.THREE_PI_OVER_TWO, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, TestValues.UNDEFINED },
        }),
        new FunctionTestCase("arccot", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.PI },
            { TestValues.ZERO, TestValues.PI_OVER_TWO },
            { TestValues.ONE, new Number(0.78539816) },
            { TestValues.NEGATIVE_ONE, TestValues.THREE_PI_OVER_FOUR },
            { TestValues.PI, new Number(0.30816907) },
            { TestValues.NEGATIVE_PI, new Number(2.8334236) },
            { TestValues.PI_OVER_FOUR, new Number(0.90502258) },
            { TestValues.NEGATIVE_PI_OVER_FOUR, new Number(2.2365701) },
            { TestValues.PI_OVER_TWO, new Number(0.5669115) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(2.5746811) },
            { TestValues.TAU, new Number(0.15783119) },
            { TestValues.NEGATIVE_TAU, new Number(2.9837614633016343) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.20910464) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(2.932488) },
        }),
        new FunctionTestCase("arccoth", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.UNDEFINED },
            { TestValues.ONE, TestValues.INFINITY },
            { TestValues.NEGATIVE_ONE, TestValues.NEGATIVE_INFINITY },
            { TestValues.PI, new Number(0.32976531) },
            { TestValues.NEGATIVE_PI, new Number(-0.32976531) },
            { TestValues.PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.NEGATIVE_PI_OVER_FOUR, TestValues.UNDEFINED },
            { TestValues.PI_OVER_TWO, new Number(0.75246927) },
            { TestValues.NEGATIVE_PI_OVER_TWO, new Number(-0.75246927) },
            { TestValues.TAU, new Number(0.16051956) },
            { TestValues.NEGATIVE_TAU, new Number(-0.16051956) },
            { TestValues.THREE_PI_OVER_TWO, new Number(0.21548086) },
            { TestValues.NEGATIVE_THREE_PI_OVER_TWO, new Number(-0.21548086) },
        }),
    ];
}
