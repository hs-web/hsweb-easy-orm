package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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
            return ((Collection<Object>) value)
                    .stream()
                    .map(val -> this.convertValue(column, val))
                    .collect(Collectors.toList());
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

        return convertValue(column, term.getValue());
    }
}
