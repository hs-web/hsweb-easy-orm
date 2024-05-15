package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;

import java.util.*;

/**
 * 抽象SQL条件片段构造器，实现{@link TermFragmentBuilder},提供常用的模版方法
 *
 * @author zhouhao
 * @see TermFragmentBuilder
 * @since 4.0
 */
@AllArgsConstructor
public abstract class AbstractTermFragmentBuilder implements TermFragmentBuilder {

    @Getter
    private final String termType;

    @Getter
    private final String name;

    /**
     * 尝试转换条件值为List,如果值为字符串则按,分割.
     *
     * @param column 列
     * @param term   条件
     * @return List值
     */
    @SuppressWarnings("all")
    protected List<Object> convertList(RDBColumnMetadata column, Term term) {
        Object value = term.getValue();
        if (value == null) {
            return Collections.emptyList();
        }
        //逗号分割自动转为list,比如在 in查询时,前端直接传入1,2,3即可.
        //todo 支持转义: 1,2\,3,4 => ["1","2,3","4"]
        if (value instanceof String) {
            value = ((String) value).split(",");
        }

        //数组
        if (value instanceof Object[]) {
            value = Arrays.asList(((Object[]) value));
        }

        //集合
        if (value instanceof Collection) {
            Collection<Object> listValue = ((Collection<Object>) value);
            List<Object> list = new ArrayList<>(listValue.size());

            for (Object val : listValue) {
                list.add(this.convertValue(column, val));
            }

            return list;
        }
        //单个值
        return Arrays.asList(this.convertValue(column, value));
    }

    protected TableOrViewMetadata getTable(String tableName, RDBColumnMetadata baseOn) {
        return baseOn
                .getOwner()
                .getSchema()
                .getTableOrView(tableName)
                .orElseThrow(() -> new UnsupportedOperationException("table " + tableName + " does not exist"));
    }

    protected String getTableName(String tableName, RDBColumnMetadata baseOn) {
        return getTable(tableName, baseOn).getFullName();
    }

    protected String buildColumnFullName(String column, RDBColumnMetadata baseOn) {
        String table = baseOn.getOwner().getName();
        return baseOn.getDialect().buildColumnFullName(table, column);
    }

    private Object convertValue(RDBColumnMetadata column, Object val) {
        if (column == null) {
            return val;
        }
        return column.encode(val);
    }

    /**
     * 根据列定义来转换值
     *
     * @param column 列
     * @param term   条件
     * @return 转换后的值
     */
    protected Object convertValue(RDBColumnMetadata column, Term term) {
        if (term.getValue() instanceof NativeSql) {
            return term.getValue();
        }
        return convertValue(column, term.getValue());
    }

    protected PrepareSqlFragments appendPrepareOrNative(PrepareSqlFragments sql, Object value) {
        if (value instanceof NativeSql) {
            NativeSql nativeSql = ((NativeSql) value);
            sql.addSql(nativeSql.getSql())
               .addParameter(nativeSql.getParameters());
        } else {
            sql.addSql("?")
               .addParameter(value);
        }
        return sql;
    }


    /**
     * 根据字段全名获取表别名
     *
     * @param columnFullName 列全名
     * @param dialect        数据库方言
     * @return 表别名
     */
    protected static Optional<String> parseTablePlainName(String columnFullName, Dialect dialect) {
        if (columnFullName.contains(".")) {
            String[] split = parsePlainName(columnFullName, dialect).split("\\.");
            return Optional.of(split[0]);
        }
        return Optional.empty();
    }

    protected static String parsePlainName(String name, Dialect dialect) {
        if (name == null || name.isEmpty()) {
            return null;
        }

        if (name.startsWith(dialect.getQuoteStart())) {
            return name
                .replace(dialect.getQuoteStart(), "")
                .replace(dialect.getQuoteEnd(), "");
        }

        return name;
    }
}
