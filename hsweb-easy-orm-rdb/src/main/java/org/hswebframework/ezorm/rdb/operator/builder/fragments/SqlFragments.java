package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * SQL片段信息,用于记录SQL片段.
 *
 * @author zhouhao
 * @see AppendableSqlFragments
 * @see BatchSqlFragments
 * @see PrepareSqlFragments
 * @see SimpleSqlFragments
 * @see SqlFragments#single(String)
 * @see SqlFragments#of(String...)
 * @since 3.0
 */
public interface SqlFragments {

    /*  常用的SQL片段常量  */
    // (
    SqlFragments LEFT_BRACKET = SqlFragments.single("(");
    // )
    SqlFragments RIGHT_BRACKET = SqlFragments.single(")");
    // ,
    SqlFragments COMMA = SqlFragments.single(",");
    // =
    SqlFragments EQUAL = SqlFragments.single("="),
        NOT_EQUAL = SqlFragments.single("!=");
    // where
    SqlFragments WHERE = SqlFragments.single("where");
    // and
    SqlFragments AND = SqlFragments.single("and");
    // or
    SqlFragments OR = SqlFragments.single("or");
    // not
    SqlFragments NOT = SqlFragments.single("not");
    // ?
    SqlFragments QUESTION_MARK = SqlFragments.single("?");

    SqlFragments ZERO = SqlFragments.single("0"),
        ONE = SqlFragments.single("1");

    /**
     * @return 是否为空
     */
    boolean isEmpty();

    /**
     * @return 是否不为空
     */
    default boolean isNotEmpty() {
        return !isEmpty();
    }

    /**
     * @return 全部SQL片段
     */
    List<String> getSql();

    /**
     * @return SQL预编译参数
     */
    List<Object> getParameters();

    /**
     * 转换为SQL请求
     *
     * @return SQL请求
     * @see SqlRequest
     */
    default SqlRequest toRequest() {
        if (isEmpty()) {
            return EmptySqlRequest.INSTANCE;
        }
        return SqlRequests.prepare(StringUtils.join(" ", getSql()), getParameters().toArray());
    }

    /**
     * 编码一个已由写入器选定的值；显式 SQL 保留原表达式，其他值只编码一次。
     * 列省略、默认值和方言限制由调用方处理，编码结果交给
     * {@link AppendableSqlFragments#addValue(Object)} 追加，不能直接当绑定参数使用。
     *
     * @param column 列元数据
     * @param value 原始值或显式 SQL
     * @return 绑定值或编码后的 SQL 表达式
     * @since 4.2.3
     */
    static Object encodeValue(RDBColumnMetadata column, Object value) {
        return value instanceof NativeSql ? value : column.encode(value);
    }

    /**
     * 包装单个SQL字符串为{@link SqlFragments}
     *
     * @param sql SQL字符串
     * @return SqlFragments
     */
    static SqlFragments single(String sql) {
        return SimpleSqlFragments.of(Collections.singletonList(sql),
                                     Collections.emptyList());
    }

    /**
     * 包装多个SQL字符串为{@link SqlFragments}
     *
     * @param sql SQL字符串
     * @return SqlFragments
     */
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
