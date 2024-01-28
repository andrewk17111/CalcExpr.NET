using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MaximumAttribute(double maximum, bool allow_undefined = false, bool inclusive = true)
    : ConditionAttribute(expression => TestCondition(expression, ((Number)maximum).Evaluate(), allow_undefined, inclusive))
{
    public readonly IExpression Maximum = ((Number)maximum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    private static bool TestCondition(IExpression expression, IExpression maximum, bool allow_undefined, bool inclusive)
    {
        IExpression condition_result = new BinaryOperator(inclusive ? "<=" : "<", expression, maximum).Evaluate();

        return condition_result is Number num && num.Value != 0 || allow_undefined;
    }
}
