using DiceEngine.Expressions;

namespace DiceEngine.FunctionAttributes.ConditionalAttributes;

public class RangeAttribute(double minimum, double maximum, bool allow_undefined = false, bool inclusive = true)
    : ConditionAttribute
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly IExpression Maximum = ((Number)maximum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression lower_condition_result = new BinaryOperator(Inclusive ? ">=" : ">", expression, Minimum).Evaluate();

        if (lower_condition_result is Number low_num)
        {
            if (low_num.Value == 0)
            {
                return false;
            }
        }
        else if (!AllowUndefined)
        {
            return false;
        }

        IExpression upper_condition_result = new BinaryOperator(Inclusive ? "<=" : "<", expression, Maximum).Evaluate();

        return upper_condition_result is Number high_num ? high_num.Value != 0 : AllowUndefined;
    }
}
