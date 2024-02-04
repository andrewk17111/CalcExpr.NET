using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MinimumAttribute(double minimum, bool allow_undefined = false, bool inclusive = true) : ConditionAttribute
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression condition_result = new BinaryOperator(Inclusive ? ">=" : ">", expression, Minimum).Evaluate();

        return condition_result is Number num && num.Value != 0 || AllowUndefined;
    }
}
