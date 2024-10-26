﻿using CalcExpr.Context;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.Expressions.Interfaces;

public interface IPrefixOperable
{
    Terminal PrefixOperate(string identifier, ExpressionContext context);

    public static Terminal Operate(string identifier, IExpression inside, ExpressionContext? context = null)
    {
        context ??= new ExpressionContext();

        if (inside is IPrefixOperable operable)
        {
            Terminal result = operable.PrefixOperate(identifier, context);

            return result;
        }
        else if (inside is IEnumerableExpression enumExpr)
        {
            return TerminalCollection.TerminateCollection(enumExpr.Map(e => Operate(identifier, e, context)));
        }

        return Undefined.UNDEFINED;
    }
}
