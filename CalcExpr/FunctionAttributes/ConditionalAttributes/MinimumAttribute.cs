using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MinimumAttribute(double minimum, bool allow_undefined = false, bool inclusive = true) : ConditionAttribute
{
    public readonly IExpression Minimum = ((Number)minimum).Evaluate();
    public readonly bool AllowUndefined = allow_undefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression condition_result = (expression as IBinaryOperable)?
            .BinaryLeftOperate(Inclusive ? ">=" : ">", Minimum, new ExpressionContext()) ?? Undefined.UNDEFINED;

        return condition_result is Logical isHigher ? isHigher.Value : AllowUndefined;
    }
}
