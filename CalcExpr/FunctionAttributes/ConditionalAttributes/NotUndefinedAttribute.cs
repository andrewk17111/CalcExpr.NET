using CalcExpr.Expressions;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class NotUndefinedAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
        => !Undefined.UNDEFINED.Equals(expression);
}
