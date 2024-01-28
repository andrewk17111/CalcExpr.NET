using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class GapAttribute(double start, double end, bool allow_undefined = false, bool inclusive = false)
    : ConditionAttribute(expression => TestCondition(expression, ((Number)start).Evaluate(),
        new Number(end).Evaluate(), allow_undefined, inclusive))
{
    public readonly IExpression Start = ((Number)start).Evaluate();
    public readonly IExpression End = ((Number)end).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    private static bool TestCondition(IExpression expression, IExpression start, IExpression end,
        bool allow_undefined, bool inclusive)
    {
        IExpression lower_condition_result = new BinaryOperator(inclusive ? "<" : "<=", expression, start).Evaluate();

        if (lower_condition_result is Number low_num)
        {
            if (low_num.Value == 1)
            {
                return true;
            }
        }
        else if (!allow_undefined)
        {
            return false;
        }

        IExpression upper_condition_result = new BinaryOperator(inclusive ? ">" : ">=", expression, end).Evaluate();

        return upper_condition_result is Number high_num ? high_num.Value == 1 : allow_undefined;
    }
}
