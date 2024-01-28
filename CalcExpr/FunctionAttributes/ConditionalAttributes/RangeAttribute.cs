using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class RangeAttribute(double minimum, double maximum, bool allow_undefined = false, bool inclusive = true)
    : ConditionAttribute(expression => TestCondition(expression, ((Number)minimum).Evaluate(),
        new Number(maximum).Evaluate(), allow_undefined, inclusive))
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly IExpression Maximum = ((Number)maximum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    private static bool TestCondition(IExpression expression, IExpression minimum, IExpression maximum,
        bool allow_undefined, bool inclusive)
    {
        IExpression lower_condition_result = new BinaryOperator(inclusive ? ">=" : ">", expression, minimum).Evaluate();

        if (lower_condition_result is Number low_num)
        {
            if (low_num.Value == 0)
            {
                return false;
            }
        }
        else if (!allow_undefined)
        {
            return false;
        }

        IExpression upper_condition_result = new BinaryOperator(inclusive ? "<=" : "<", expression, maximum).Evaluate();

        return upper_condition_result is Number high_num ? high_num.Value != 0 : allow_undefined;
    }
}
