package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

@Getter
@Setter
@AllArgsConstructor(staticName = "of")
public class NativeSqlDefaultValue implements DefaultValue, NativeSql {
    private String sql;
    @Override
    public Object get() {
        return null;
    }

    @Override
    public String getSql() {
        return sql;
    }

    @Override
    public Object[] getParameters() {
        return new Object[0];
    }
}
