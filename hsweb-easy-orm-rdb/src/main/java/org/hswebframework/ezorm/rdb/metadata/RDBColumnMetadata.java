package org.hswebframework.ezorm.rdb.metadata;

import lombok.*;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.*;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.*;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RDBColumnMetadata extends AbstractColumnMetadata implements ColumnMetadata, Cloneable, Comparable<RDBColumnMetadata> {

    /**
     * 数据类型,如:varchar(32)
     *
     * @since 1.0
     */
    private String dataType;

    /**
     * 长度
     *
     * @since 1.1
     */
    private int length;

    /**
     * 精度
     *
     * @since 1.1
     */
    private int precision;

    /**
     * 小数位数
     *
     * @since 1.1
     */
    private int scale;

    /**
     * 是否主键
     */
    private boolean primaryKey;

    /**
     * 自定义都列定义,使用它后其他列相关设置将无效
     *
     * @since 3.0
     */
    private String columnDefinition;

    /**
     * 是否可以更新
     *
     * @since 4.0
     */
    private boolean updatable = true;

    /**
     * 是否可以新增
     *
     * @since 4.0
     */
    private boolean insertable = true;

    /**
     * 是否可以保存，用于upsert时的更新
     *
     * @since 4.0.12
     */
    private boolean saveable = true;

    /**
     * 是否自增
     *
     * @since 4.0.14
     */
    private boolean autoIncrement = false;

    /**
     * DataType
     *
     * @since 4.0
     */
    private DataType type;

    /**
     * 排序序号
     */
    private int sortIndex;

    /**
     * 所有者
     */
    private TableOrViewMetadata owner;

    /**
     * 曾经的名字
     */
    private String previousName;


    public Dialect getDialect() {
        return getOwner().getDialect();
    }

    public String getQuoteName() {
        return getDialect().quote(getName());
    }

    public void setJdbcType(SQLType jdbcType, Class<?> javaType) {
        this.javaType = javaType;
        setType(JdbcDataType.of(jdbcType, javaType));
    }

    public int getPrecision(int defaultPrecision) {
        if (precision <= 0) {
            if (length <= 0) {
                return defaultPrecision;
            } else {
                return length;
            }
        }
        return precision;
    }

    public void setType(DataType dataType) {
        this.javaType = dataType.getJavaType();
        this.type = dataType;
    }

    @Override
    public Class<?> getJavaType() {
        if (javaType == null && type != null) {
            return javaType = type.getJavaType();
        }
        return super.getJavaType();
    }

    public String getDataType() {
        if (dataType != null) {
            return dataType;
        }
        return getDialect().buildColumnDataType(this);
    }

    public SQLType getSqlType() {
        return Optional.ofNullable(type)
                       .map(DataType::getSqlType)
                       .orElse(null);
    }

    public String getPreviousName() {
        if (previousName == null) {
            previousName = name;
        }
        return previousName;
    }

    @Override
    public int compareTo(RDBColumnMetadata target) {
        return Integer.compare(sortIndex, target.getSortIndex());
    }

    @Override
    @SuppressWarnings("all")
    @SneakyThrows
    public RDBColumnMetadata clone() {
        RDBColumnMetadata columnMetadata = ((RDBColumnMetadata) super.clone());
        columnMetadata.setProperties(new HashMap<>(getProperties()));
        columnMetadata.setFeatures(new HashMap<>(getFeatures()));

        return columnMetadata;
    }

    @Override
    public Object encode(Object data) {
        if (data instanceof NullValue) {
            return data;
        }
        return super.encode(data);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(name);
        builder.append(" ").append(type != null ? getDataType() : "");

        if (javaType != null) {
            builder.append(" ").append(javaType.getSimpleName());
        }

        if (comment != null && !comment.isEmpty()) {
            builder.append(" /*").append(comment).append("*/");
        }

        return builder.toString();
    }

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.column;
    }

    @Override
    public <T extends Feature> T findFeatureOrElse(String id, Supplier<T> orElse) {
        T current = getFeatureOrElse(id, null);
        if (null != current) {
            return current;
        }
        if (owner != null) {
            return owner.findFeatureOrElse(id, null);
        }
        return orElse == null ? null : orElse.get();
    }

    public List<Feature> findFeatures(Predicate<Feature> predicate) {
        return Stream.concat(owner.findFeatures().stream(), getFeatureList().stream())
                     .filter(predicate)
                     .collect(Collectors.toList());
    }

    public String getFullName(String ownerName) {
        if (ownerName == null || ownerName.isEmpty()) {
            ownerName = getOwner().getName();
        }
        return getDialect().buildColumnFullName(ownerName, getName());
    }

    public String getFullName() {
        return getFullName(getOwner().getName());
    }

    public String getFullTableName() {
        return getFullName(getOwner().getFullName());
    }

    public boolean ddlModifiable(RDBColumnMetadata after) {
        if (!this.getName().equals(this.getPreviousName())) {
            return true;
        }
        if (!this.isNotNull() == after.isNotNull()) {
            return true;
        }
        DataType type = getType();
        if (type != null) {
            if (getDialect().buildColumnDataType(this).equals(getDialect().buildColumnDataType(after))) {
                return false;
            }
            if (type.isLengthSupport()) {
                return type.isNumber()
                        ? getPrecision() < after.getPrecision() || getScale() < after.getScale()
                        : getLength() < after.getLength()
                        ;
            }
            if (type.isScaleSupport()) {
                return getScale() < after.getScale();
            }
            return false;
        }
        return false;
    }

    public void setLength(int length) {
        this.length = length;
        this.setDataType(null);
    }

    public Optional<Object> generateDefaultValue() {
        return Optional.ofNullable(defaultValue)
                       .filter(RuntimeDefaultValue.class::isInstance)
                       .map(RuntimeDefaultValue.class::cast)
                       .map(defaultValue -> decode(defaultValue.get()));
    }

    @Override
    public boolean isNotNull() {
        return isPrimaryKey() || super.isNotNull();
    }
}
