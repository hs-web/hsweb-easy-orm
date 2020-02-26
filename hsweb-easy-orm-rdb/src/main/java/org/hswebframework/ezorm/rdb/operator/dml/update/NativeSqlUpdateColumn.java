package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

@Getter
@Setter
public class NativeSqlUpdateColumn extends UpdateColumn implements NativeSql {

    private String sql;

    private Object[] parameters;

    public static NativeSqlUpdateColumn of(String column, String sql, Object... parameters) {
        NativeSqlUpdateColumn updateColumn = new NativeSqlUpdateColumn();
        updateColumn.setSql(sql);
        updateColumn.setColumn(column);
        updateColumn.setParameters(parameters);
        return updateColumn;
    }
}
