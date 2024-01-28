using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class ConditionAttribute(Func<IExpression, bool> condition) : FunctionAttribute
{
    public readonly Func<IExpression, bool> CheckCondition = condition;
}
