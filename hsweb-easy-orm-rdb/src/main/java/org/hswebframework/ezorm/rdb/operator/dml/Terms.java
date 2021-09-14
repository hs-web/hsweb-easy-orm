package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;

import java.util.function.Consumer;

public interface Terms {

    interface Enums {
        String inAny = "in$any";
        String notInAny = "nin$any";
    }

    /**
     * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.term.LikeTermFragmentBuilder
     */
    interface Like {

        static Term of(String column,
                       String value) {
            Term term = new Term();
            term.setTermType(TermType.like);
            term.setColumn(column);
            term.setValue(value);
            return term;
        }

        static <T> Term of(StaticMethodReferenceColumn<T> column,
                           String value) {
            return of(column.getColumn(), value);
        }

        static <T> Term reversal(StaticMethodReferenceColumn<T> column,
                                 String value,
                                 boolean startWith,
                                 boolean endWith) {
            return reversal(column.getColumn(), value, startWith, endWith);
        }

        static Term reversal(String column,
                             String value,
                             boolean startWith,
                             boolean endWith) {
            Term term = of(column, value);
            term.getOptions().add("reversal");
            if (startWith) {
                term.getOptions().add("startWith");
            }
            if (endWith) {
                term.getOptions().add("endWith");
            }
            return term;
        }
    }

    static <B, T extends Conditional<?>> Consumer<T> enumInAny(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, Enums.inAny, enums);
    }

    static <B, T extends Conditional<?>> Consumer<T> enumNotInAny(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, Enums.notInAny, enums);
    }

}
