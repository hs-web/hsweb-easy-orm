package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

@Getter
@Setter
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NativeSelectColumn extends SelectColumn implements NativeSql {
    private String sql;

    private Object[] parameters;

    public NativeSelectColumn(String sql) {
        this.sql = sql;
    }

    public static NativeSelectColumn of(String sql) {
        return new NativeSelectColumn(sql);
    }

    public static NativeSelectColumn of(String sql, Object... parameters) {
        return new NativeSelectColumn(sql, parameters);
    }


}
