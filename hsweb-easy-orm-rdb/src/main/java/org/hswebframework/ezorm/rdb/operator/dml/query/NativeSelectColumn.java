package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

@Getter
@Setter
@AllArgsConstructor
public class NativeSelectColumn extends SelectColumn implements NativeSql {
    private String sql;

    public static NativeSelectColumn of(String sql){
        return new NativeSelectColumn(sql);
    }

}
