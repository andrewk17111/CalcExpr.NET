using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public abstract class ConditionAttribute : FunctionAttribute
{
    public abstract bool CheckCondition(IExpression expression);
}
