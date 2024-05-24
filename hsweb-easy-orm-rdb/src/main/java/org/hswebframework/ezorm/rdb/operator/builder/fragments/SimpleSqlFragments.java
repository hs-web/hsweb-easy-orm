package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@AllArgsConstructor(staticName = "of")
public class SimpleSqlFragments implements SqlFragments {
    private final List<String> sql;
    private final List<Object> parameters;

    public static SqlFragments of(String sql, Object... args) {
        return of(Collections.singletonList(sql), Arrays.asList(args));
    }

    @Override
    public boolean isEmpty() {
        return sql.isEmpty();
    }

    @Override
    public List<String> getSql() {
        return sql;
    }

    @Override
    public List<Object> getParameters() {
        return parameters;
    }
}
