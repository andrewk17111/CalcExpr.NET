using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MinimumAttribute(double minimum, bool allow_undefined = false, bool inclusive = true)
    : ConditionAttribute(expression => TestCondition(expression, ((Number)minimum).Evaluate(), allow_undefined, inclusive))
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    private static bool TestCondition(IExpression expression, IExpression minimum, bool allow_undefined, bool inclusive)
    {
        IExpression condition_result = new BinaryOperator(inclusive ? ">=" : ">", expression, minimum).Evaluate();

        return condition_result is Number num && num.Value != 0 || allow_undefined;
    }
}
