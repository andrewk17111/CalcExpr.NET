using CalcExpr.Expressions;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class IsNumberAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
        => expression is Number;
}
