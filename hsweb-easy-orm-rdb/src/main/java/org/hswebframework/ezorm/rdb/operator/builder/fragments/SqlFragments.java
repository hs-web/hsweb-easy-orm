package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public interface SqlFragments {

    SqlFragments LEFT_BRACKET = SqlFragments.single("(");
    SqlFragments RIGHT_BRACKET = SqlFragments.single(")");
    SqlFragments COMMA = SqlFragments.single(",");
    SqlFragments EQUAL = SqlFragments.single("=");
    SqlFragments WHERE = SqlFragments.single("where");
    SqlFragments AND = SqlFragments.single("and");
    SqlFragments OR = SqlFragments.single("or");
    SqlFragments NOT = SqlFragments.single("not");
    SqlFragments QUESTION_MARK = SqlFragments.single("?");

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
        return SqlRequests.prepare(StringUtils.join(" ", getSql()), getParameters().toArray());
    }

    static SqlFragments single(String sql) {
        return SimpleSqlFragments.of(Collections.singletonList(sql),
                                     Collections.emptyList());
    }

    static SqlFragments of(String... sql) {
        if (sql == null || sql.length == 0) {
            return EmptySqlFragments.INSTANCE;
        }
        if (sql.length == 1) {
            return single(sql[0]);
        }
        return SimpleSqlFragments
            .of(Arrays.asList(sql),
                Collections.emptyList());
    }


}
