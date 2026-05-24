package org.hswebframework.ezorm.rdb.supports.json;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collections;
import java.util.List;

@Getter
@AllArgsConstructor(staticName = "of")
public class JsonScalarExpression {

    private final String sql;

    private final List<Object> parameters;

    public static JsonScalarExpression of(String sql) {
        return of(sql, Collections.emptyList());
    }
}
