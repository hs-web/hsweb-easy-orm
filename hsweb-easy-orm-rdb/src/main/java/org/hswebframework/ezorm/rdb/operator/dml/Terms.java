package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.TermType;

import java.util.function.Consumer;

public interface Terms {

    static <B,T extends Conditional<?>> Consumer<T> enumNotIn(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, TermType.nin, enums);
    }

    static <B,T extends Conditional<?>> Consumer<T> enumIn(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, TermType.in, enums);
    }

    static <B,T extends Conditional<?>> Consumer<T> enumInAny(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, "in$any", enums);
    }

    static <B,T extends Conditional<?>> Consumer<T> enumNotInAny(StaticMethodReferenceColumn<B> column, Object... enums) {
        String columnName = column.getColumn();
        return (cdt) -> cdt.accept(columnName, "nin$any", enums);
    }

}
