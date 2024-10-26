using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class MinimumAttribute(double minimum, bool allowUndefined = false, bool inclusive = true) : ConditionAttribute
{
    public readonly Terminal Minimum = (Terminal)minimum;
    public readonly bool AllowUndefined = allowUndefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression conditionResult = IBinaryOperable.Operate(Inclusive ? ">=" : ">", expression, Minimum);

        return conditionResult is Logical isHigher ? isHigher.Value : AllowUndefined;
    }
}
