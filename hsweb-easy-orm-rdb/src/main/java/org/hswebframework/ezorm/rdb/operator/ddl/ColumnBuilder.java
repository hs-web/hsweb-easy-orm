package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.NativeSqlDefaultValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.Date;
import java.util.function.Consumer;

/**
 * 列构造器,用于通过DSL方式来描述列的结构:
 * <pre>{@code
 *
 *    createOrAlter("test")
 *    .addColumn("id").varchar().primaryKey().commit()
 *    .commit()
 *    .sync();
 *
 * }</pre>
 *
 * @author zhouhao
 * @since 4.0
 */
public interface ColumnBuilder {
    /**
     * 自定义列结构
     * <pre>{@code
     *   custom(column-> column.addFeature....)
     * }</pre>
     *
     * @param consumer 自定义回调
     * @return ColumnBuilder
     */
    ColumnBuilder custom(Consumer<RDBColumnMetadata> consumer);

    /**
     * 设置列名 {@link RDBColumnMetadata#getName()}
     *
     * @param name 列名
     * @return ColumnBuilder
     */
    ColumnBuilder name(String name);

    /**
     * 设置别名 {@link RDBColumnMetadata#getAlias()}
     *
     * @param name 别名
     * @return ColumnBuilder
     */
    ColumnBuilder alias(String name);

    /**
     * 设置数据类型,如: varchar(32) {@link RDBColumnMetadata#getDataType()}.
     * <p>
     * 建议使用{@link ColumnBuilder#type(JDBCType, Class)}和{@link ColumnBuilder#length(int)}
     *
     * @param dataType 数据类型
     * @return ColumnBuilder
     */
    ColumnBuilder dataType(String dataType);

    /**
     * 根据类型ID设置类型
     *
     * @param typeId 类型ID
     * @return ColumnBuilder
     * @see Dialect#convertDataType(String)
     */
    ColumnBuilder type(String typeId);

    /**
     * 设置数据类型
     *
     * @param type 类型
     * @return ColumnBuilder
     * @see Dialect#convertDataType(String)
     */
    ColumnBuilder type(DataType type);

    /**
     * 设置列注释
     *
     * @param comment 列注释
     * @return ColumnBuilder
     */
    ColumnBuilder comment(String comment);

    /**
     * 设置不能为空 {@link RDBColumnMetadata#isNotNull()}
     *
     * @return ColumnBuilder
     */
    ColumnBuilder notNull();

    /**
     * 设置为主键 {@link RDBColumnMetadata#isPrimaryKey()}
     *
     * @return ColumnBuilder
     */
    ColumnBuilder primaryKey();

    /**
     * 设置固定的列定义 {@link RDBColumnMetadata#setColumnDefinition(String)}
     * <pre>{@code
     * columnDef("varchar(32) not null")
     * }</pre>
     *
     * @param def 列定义
     * @return ColumnBuilder
     */
    ColumnBuilder columnDef(String def);

    /**
     * 设置默认值
     *
     * @param value 默认值
     * @return ColumnBuilder
     * @see NativeSqlDefaultValue
     * @see RuntimeDefaultValue
     */
    ColumnBuilder defaultValue(DefaultValue value);

    /**
     * 自定义列配置 {@link RDBColumnMetadata#setProperty(String, Object)}
     *
     * @param propertyName 配置名
     * @param value        值
     * @return ColumnBuilder
     */
    ColumnBuilder property(String propertyName, Object value);

    /**
     * 设置列长度 ,通常用于字符类型的列{@link RDBColumnMetadata#setLength(int)}
     *
     * @param len 列长度
     * @return ColumnBuilder
     */
    ColumnBuilder length(int len);

    /**
     * 设置列位数和精度,通常用于数值类型的列. {@link RDBColumnMetadata#setPrecision(int)}
     *
     * @param precision 位数
     * @param scale     精度 (小数位数)
     * @return ColumnBuilder
     */
    ColumnBuilder length(int precision, int scale);

    /**
     * 完成构造,返回TableBuilder
     *
     * @return TableBuilder
     */
    TableBuilder commit();

    /**
     * 设置JDBC类型和JAVA类型
     *
     * @param jdbcType JDBC类型
     * @param javaType JAVA类型
     * @return ColumnBuilder
     */
    default ColumnBuilder type(JDBCType jdbcType, Class<?> javaType) {
        return type(DataType.jdbc(jdbcType, javaType));
    }

    /**
     * 设置字符串类型
     *
     * @param length 长度
     * @return ColumnBuilder
     */
    default ColumnBuilder varchar(int length) {
        return type(JDBCType.VARCHAR, String.class).length(length);
    }

    /**
     * 设置数字类型 {@link JDBCType#NUMERIC} {@link BigDecimal}
     *
     * @param precision 数字位数
     * @param scale     小数位数
     * @return ColumnBuilder
     */
    default ColumnBuilder number(int precision, int scale) {
        return type(JDBCType.NUMERIC, BigDecimal.class).length(precision, scale);
    }

    /**
     * 设置整数类型 {@link JDBCType#NUMERIC} {@link Long}
     *
     * @param len 数字位数
     * @return ColumnBuilder
     */
    default ColumnBuilder number(int len) {
        return type(JDBCType.NUMERIC, Long.class).length(len, 0);
    }

    /**
     * 设置文本类型,{@link JDBCType#LONGNVARCHAR}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder text() {
        return type(JDBCType.LONGVARCHAR, String.class);
    }

    /**
     * 设置CLOB类型,{@link JDBCType#CLOB}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder clob() {
        return type(JDBCType.CLOB, String.class);
    }

    /**
     * 设置integer类型,{@link JDBCType#INTEGER}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder integer() {
        return type(JDBCType.INTEGER, Integer.class);
    }

    /**
     * 设置BIGINT类型,{@link JDBCType#BIGINT}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder bigint() {
        return type(JDBCType.BIGINT, Long.class);
    }

    /**
     * 设置TINYINT类型,{@link JDBCType#TINYINT}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder tinyint() {
        return type(JDBCType.TINYINT, Byte.class);
    }

    /**
     * 设置TIMESTAMP类型,{@link JDBCType#TIMESTAMP}
     *
     * @return ColumnBuilder
     */
    default ColumnBuilder datetime() {
        return type(JDBCType.TIMESTAMP, Date.class);
    }

    /**
     * 设置默认值SQL
     *
     * @param defaultSql 默认值SQL
     * @return ColumnBuilder
     */
    default ColumnBuilder defaultValueNative(String defaultSql) {
        return defaultValue(NativeSqlDefaultValue.of(defaultSql));
    }

    /**
     * 设置运行时默认值
     *
     * @param value 默认值
     * @return ColumnBuilder
     */
    default ColumnBuilder defaultValueRuntime(RuntimeDefaultValue value) {
        return defaultValue(value);
    }

}
