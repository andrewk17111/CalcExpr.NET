using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;

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
        IBinaryOperable? binaryOperable = expression as IBinaryOperable;
        IExpression lower_condition_result = binaryOperable?.BinaryLeftOperate(Inclusive ? "<" : "<=", Start,
            new ExpressionContext()) ?? Undefined.UNDEFINED;

        if (lower_condition_result is Logical isLower)
        {
            if (isLower.Value)
            {
                return true;
            }
        }
        else if (!AllowUndefined)
        {
            return false;
        }

        IExpression upper_condition_result = binaryOperable?.BinaryLeftOperate(Inclusive ? ">" : ">=", End,
            new ExpressionContext()) ?? Undefined.UNDEFINED;

        return upper_condition_result is Logical isHigher ? isHigher.Value : AllowUndefined;
    }
}
