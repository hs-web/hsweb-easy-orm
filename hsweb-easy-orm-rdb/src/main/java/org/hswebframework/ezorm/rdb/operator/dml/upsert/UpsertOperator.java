package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * upsert 操作器,用于执行upsert请求
 *
 * @author zhouhao
 * @since 4.0
 */
public abstract class UpsertOperator {

    /**
     * 设置列,和{@link this#values(Object...)}配合使用,如:
     * <pre>
     *     operator.columns("id","name")
     *              .values(id,name);
     * </pre>
     *
     * @param columns 列名
     * @return this
     */
    public abstract UpsertOperator columns(String... columns);

    /**
     * 设置值列表,与{@link this#columns(String...)}配合使用
     *
     * @param values 值列表
     * @return this
     */
    public abstract UpsertOperator values(Object... values);

    /**
     * 忽略更新的列,如果是执行update,则忽略更新指定的列.
     *
     * @param columns 列名
     * @return this
     * @see UpsertColumn#isUpdateIgnore()
     */
    public abstract UpsertOperator ignoreUpdate(String... columns);

    /**
     * 与{@link this#value(String, Object, boolean)}相同.默认不忽略更新.
     *
     * @param column 列名
     * @param value  值
     * @return 值
     */
    public abstract UpsertOperator value(String column, Object value);

    /**
     * 设置列名和值,不能与{@link this#columns(String...)}一起用,同时设置是否忽略更新列.
     * 忽略更新的列在更新的时候不会执行更新.
     *
     * @param column       列名
     * @param value        值
     * @param ignoreUpdate 是否忽略更新列.
     * @return this
     */
    public abstract UpsertOperator value(String column, Object value, boolean ignoreUpdate);

    /**
     * 使用静态方法引用来描述列名,与{@link this#columns(String...)}一样的效果.
     * 例如:
     * <pre>
     *     operator.columns(User::getId,User::getName)
     *              .values(id,name);
     * </pre>
     *
     * @param columns 方法引用列
     * @param <T>     方法所在类的类型
     * @return this
     * @see StaticMethodReferenceColumn
     */
    @SafeVarargs
    public final <T> UpsertOperator columns(StaticMethodReferenceColumn<T>... columns) {
        return columns(Arrays.stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    /**
     * 使用方法引用来描述列和值.例如:
     * <pre>
     *     User user = ....;
     *     operator.values(user::getId,user::getName);
     *
     * </pre>
     *
     * @param columns 列名
     * @param <T>     方法所在类的类型
     * @return this
     * @see MethodReferenceColumn
     */
    @SafeVarargs
    public final <T> UpsertOperator values(MethodReferenceColumn<T>... columns) {
        String[] column = new String[columns.length];
        Object[] value = new Object[columns.length];

        for (int i = 0; i < columns.length; i++) {
            column[i] = columns[i].getColumn();
            value[i] = columns[i].get();
        }

        return columns(column).values(value);
    }

    /**
     * 使用Map来描述列和值,Map的key为列名,Map的值为值
     *
     * @param values map
     * @return this
     */
    public UpsertOperator value(Map<String, Object> values) {
        values.forEach(this::value);
        return this;
    }

    /**
     * 使用Map来描述列和多个值
     *
     * @param values 多个值
     * @return this
     */
    public abstract UpsertOperator values(List<Map<String, Object>> values);

    /**
     * 执行,调用此方法不会立即执行sql,
     * 需要调用{@link SaveResultOperator#sync()}或者{@link SaveResultOperator#reactive()}来执行并获取结果.
     *
     * @return 结果操作器
     */
    public abstract SaveResultOperator execute();

}
