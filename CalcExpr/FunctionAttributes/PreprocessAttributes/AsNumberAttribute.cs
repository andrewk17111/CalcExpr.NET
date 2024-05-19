using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public class AsNumberAttribute : PreprocessAttribute
{
    public override IExpression Preprocess(IExpression expression)
        => expression is Number
            ? expression
            : Constant.TRUE.Equals(expression) || Constant.FALSE.Equals(expression)
                ? expression.Evaluate()
                : Constant.UNDEFINED;
}
