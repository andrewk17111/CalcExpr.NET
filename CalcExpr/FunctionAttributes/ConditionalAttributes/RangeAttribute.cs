using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class RangeAttribute(double minimum, double maximum, bool allow_undefined = false, bool inclusive = true)
    : ConditionAttribute
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly IExpression Maximum = ((Number)maximum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IBinaryOperable? binaryOperable = expression as IBinaryOperable;
        IExpression lower_condition_result = binaryOperable?.BinaryLeftOperate(Inclusive ? ">=" : ">", Minimum,
            new ExpressionContext()) ?? Undefined.UNDEFINED;

        if (lower_condition_result is Logical isHigher)
        {
            if (!isHigher.Value)
            {
                return false;
            }
        }
        else if (!AllowUndefined)
        {
            return false;
        }

        IExpression upper_condition_result = binaryOperable?.BinaryLeftOperate(Inclusive ? "<=" : "<", Maximum,
            new ExpressionContext()) ?? Undefined.UNDEFINED;

        return upper_condition_result is Logical isLower ? isLower.Value : AllowUndefined;
    }
}
