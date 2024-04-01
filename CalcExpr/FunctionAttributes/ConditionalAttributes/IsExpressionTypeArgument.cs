using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class IsExpressionTypeAttribute(Type type) : ConditionAttribute
{
    public readonly Type ParameterType = type;

    public override bool CheckCondition(IExpression expression)
        => expression.GetType() == ParameterType;
}
