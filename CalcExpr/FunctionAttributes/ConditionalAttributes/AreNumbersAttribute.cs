using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class AreNumbersAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
    {
        if (expression is IEnumerableExpression enum_expr)
        {
            foreach (IExpression expr in enum_expr)
                if (expr is not Number)
                    return false;

            return true;
        }

        return expression is Number;
    }
}
