package org.hswebframework.ezorm.rdb.metadata;

import lombok.*;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.AbstractColumnMetadata;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.*;
import java.util.function.Predicate;
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

    public void setJdbcType(SQLType jdbcType, Class javaType) {
        this.javaType = javaType;
        setType(JdbcDataType.of(jdbcType, javaType));
    }

    public void setType(DataType dataType) {
        this.javaType = dataType.getJavaType();
        this.type = dataType;
    }

    @Override
    public Class getJavaType() {
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
    public String toString() {
        StringBuilder builder = new StringBuilder(name);
        builder.append(" ").append(getDataType());

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

    public <T extends Feature> Optional<T> findFeature(FeatureId<T> id) {
        return findFeature(id.getId());
    }

    public <T extends Feature> Optional<T> findFeature(String id) {
        return of(this.<T>getFeature(id))
                .filter(Optional::isPresent)
                .orElseGet(() -> owner.findFeature(id));
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

    public boolean isChanged(RDBColumnMetadata after) {

        return !this.getName().equals(this.getPreviousName())
                || !this.getType().getId().equals(after.getType().getId())
                || (this.getDataType() != null && !this.getDataType().equals(after.getDataType()))
                || this.getLength() != after.getLength()
                || this.getScale() != after.getScale()
                || (this.getColumnDefinition() != null && !this.getColumnDefinition().equals(after.getColumnDefinition()));
    }

    public void setLength(int length) {
        this.length = length;
        this.setDataType(null);
    }


}
