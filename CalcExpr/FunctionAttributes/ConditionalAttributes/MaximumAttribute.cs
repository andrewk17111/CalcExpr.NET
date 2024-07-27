using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MaximumAttribute(double maximum, bool allow_undefined = false, bool inclusive = true) : ConditionAttribute
{
    public readonly IExpression Maximum = ((Number)maximum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression condition_result = (expression as IBinaryOperable)?
            .BinaryLeftOperate(Inclusive ? "<=" : "<", Maximum, new ExpressionContext()) ?? Undefined.UNDEFINED;

        return condition_result is Logical isLower ? isLower.Value : AllowUndefined;
    }
}
