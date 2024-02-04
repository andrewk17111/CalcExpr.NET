using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class GapAttribute(double start, double end, bool allow_undefined = false, bool inclusive = false)
    : ConditionAttribute
{
    public readonly IExpression Start = ((Number)start).Evaluate();
    public readonly IExpression End = ((Number)end).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression lower_condition_result = new BinaryOperator(Inclusive ? "<" : "<=", expression, Start).Evaluate();

        if (lower_condition_result is Number low_num)
        {
            if (low_num.Value == 1)
            {
                return true;
            }
        }
        else if (!AllowUndefined)
        {
            return false;
        }

        IExpression upper_condition_result = new BinaryOperator(Inclusive ? ">" : ">=", expression, End).Evaluate();

        return upper_condition_result is Number high_num ? high_num.Value == 1 : AllowUndefined;
    }
}
