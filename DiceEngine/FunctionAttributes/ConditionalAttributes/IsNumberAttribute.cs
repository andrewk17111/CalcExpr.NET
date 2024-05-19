using DiceEngine.Expressions;

namespace DiceEngine.FunctionAttributes.ConditionalAttributes;

public class IsNumberAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
        => expression is Number;
}
