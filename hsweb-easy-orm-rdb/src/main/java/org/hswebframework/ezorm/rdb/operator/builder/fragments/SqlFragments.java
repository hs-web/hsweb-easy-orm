package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;

import java.util.Collections;
import java.util.List;

public interface SqlFragments {

    boolean isEmpty();

    default boolean isNotEmpty() {
        return !isEmpty();
    }

    List<String> getSql();

    List<Object> getParameters();

    default SqlRequest toRequest() {
        if (isEmpty()) {
            return EmptySqlRequest.INSTANCE;
        }
        return SqlRequests.prepare(String.join(" ", getSql()), getParameters().toArray());
    }

    static SqlFragments single(String sql) {
        return new SqlFragments() {
            @Override
            public boolean isEmpty() {
                return false;
            }

            @Override
            public List<String> getSql() {
                return Collections.singletonList(sql);
            }

            @Override
            public List<Object> getParameters() {
                return Collections.emptyList();
            }

            @Override
            public String toString() {
                return sql;
            }
        };
    }
}
