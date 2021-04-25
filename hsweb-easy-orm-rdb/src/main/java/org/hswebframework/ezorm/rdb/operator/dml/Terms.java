package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;

import java.util.function.Consumer;

public interface Terms {

    interface Enums {
        String inAny = "in$any";
        String notInAny = "nin$any";
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
