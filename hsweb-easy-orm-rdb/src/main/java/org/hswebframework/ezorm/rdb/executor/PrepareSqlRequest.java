package org.hswebframework.ezorm.rdb.executor;

import lombok.*;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;
import org.hswebframework.utils.time.DateFormatter;

import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
public class PrepareSqlRequest implements SqlRequest {

    private String sql;

    private Object[] parameters;

    @Override
    public boolean isEmpty() {
        return sql == null || sql.isEmpty();
    }

    public String toNativeSql() {

        return SqlUtils.toNativeSql(sql, parameters);
    }

    @Override
    public String toString() {
        if (isEmpty()) {
            return "empty sql";
        }
        return toNativeSql();
    }
}
