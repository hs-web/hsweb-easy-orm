package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.h2.H2Dialect;
import org.hswebframework.ezorm.rdb.supports.kingbase.mysql.KingbaseMysqlDialect;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.sql.SQLType;
import java.util.Optional;

/**
 * 数据库方言
 *
 * @see DefaultDialect
 * @see MysqlDialect
 * @see OracleDialect
 * @see H2Dialect
 * @see PostgresqlDialect
 * @since 1.0
 */
public interface Dialect extends Feature {

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.dialect;
    }

    void addDataTypeBuilder(String typeId, DataTypeBuilder mapper);

    String buildColumnDataType(RDBColumnMetadata columnMetaData);

    String getQuoteStart();

    String getQuoteEnd();

    String clearQuote(String string);

    boolean isColumnToUpperCase();

    Optional<SQLType> convertSqlType(Class<?> type);

    DataType convertDataType(String dataType);

    default String quote(String keyword, boolean changeCase) {
        if (keyword.startsWith(getQuoteStart()) && keyword.endsWith(getQuoteEnd())) {
            return keyword;
        }
        return StringUtils.concat(
            getQuoteStart(),
            isColumnToUpperCase() && changeCase ? keyword.toUpperCase() : keyword,
            getQuoteEnd()
        );
    }

    default String quote(String keyword) {
        return quote(keyword, true);
    }

    default String buildColumnFullName(String tableName, String columnName) {
        return buildColumnFullName(tableName, columnName, true);
    }

    default String buildColumnFullName(String tableName, String columnName, boolean changeCase) {
        if (columnName.contains(".")) {
            return columnName;
        }
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), changeCase && isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), changeCase && isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    /**
     * 位运算AND操作,用于枚举类型的查询等操作
     *
     * @param column 列名
     * @param value  位值
     * @return SQL片段
     */
    default SqlFragments bitAnd(String column, long value) {
        return SqlFragments.of(column, "&", String.valueOf(value));
    }

    /**
     * 构造LIKE条件。
     *
     * <p>默认通过{@code lower}包裹左右表达式实现忽略大小写。方言可以覆盖此方法，
     * 例如使用原生的{@code ilike}操作符。</p>
     *
     * @param left       左表达式
     * @param right      右表达式
     * @param not        是否为NOT LIKE
     * @param ignoreCase 是否忽略大小写
     * @return SQL片段
     * @since 4.2.1
     */
    default SqlFragments buildLike(SqlFragments left,
                                   SqlFragments right,
                                   boolean not,
                                   boolean ignoreCase) {
        SqlFragments leftExpression = ignoreCase ? buildLower(left) : left;
        SqlFragments rightExpression = ignoreCase ? buildLower(right) : right;

        BatchSqlFragments fragments = new BatchSqlFragments(5, left.getParameters().size() + right.getParameters().size());
        fragments.add(leftExpression);
        if (not) {
            fragments.add(SqlFragments.NOT);
        }
        return fragments
            .addSql("like")
            .add(rightExpression);
    }

    /**
     * 构造小写函数表达式。
     *
     * @param expression 表达式
     * @return SQL片段
     * @since 4.2.1
     */
    default SqlFragments buildLower(SqlFragments expression) {
        if (expression == null || expression.isEmpty()) {
            return EmptySqlFragments.INSTANCE;
        }
        return new BatchSqlFragments(3, expression.getParameters().size())
            .addSql("lower(")
            .add(expression)
            .addSql(")");
    }

    /**
     * 构造字符串连接表达式。
     *
     * <p>单个表达式直接返回，多个表达式使用两参数CONCAT嵌套，避免部分方言不支持
     * 一参数或多参数CONCAT。</p>
     *
     * @param expressions 待连接的表达式
     * @return SQL片段
     * @since 4.2.1
     */
    default SqlFragments buildConcat(SqlFragments... expressions) {
        if (expressions == null || expressions.length == 0) {
            return EmptySqlFragments.INSTANCE;
        }
        SqlFragments result = expressions[0];
        for (int i = 1; i < expressions.length; i++) {
            result = new BatchSqlFragments(5, result.getParameters().size() + expressions[i].getParameters().size())
                .addSql("concat(")
                .add(result)
                .addSql(",")
                .add(expressions[i])
                .addSql(")");
        }
        return result;
    }

    Dialect MYSQL = new MysqlDialect();
    Dialect ORACLE = new OracleDialect();
    Dialect H2 = new H2Dialect();
    Dialect MSSQL = new SqlServerDialect();
    Dialect POSTGRES = new PostgresqlDialect();
    Dialect KINGBASE_MYSQL = new KingbaseMysqlDialect();

}
