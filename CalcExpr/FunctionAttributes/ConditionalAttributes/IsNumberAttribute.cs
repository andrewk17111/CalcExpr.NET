using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class IsNumberAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
        => expression is Number;
}
