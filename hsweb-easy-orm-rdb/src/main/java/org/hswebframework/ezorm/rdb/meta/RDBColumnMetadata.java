package org.hswebframework.ezorm.rdb.meta;

import lombok.*;
import org.hswebframework.ezorm.core.meta.AbstractColumnMetadata;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

import java.sql.JDBCType;
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
     * 是否不能为空
     */
    private boolean notNull;

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
    private boolean updatable;

    /**
     * JDBC Type
     */
    private JDBCType jdbcType;

    /**
     * 排序序号
     */
    private int sortIndex;

    /**
     * 所有者
     */
    private TableOrViewMetadata owner;

    public Dialect getDialect() {
        return getOwner().getDialect();
    }

    @Override
    public int compareTo(RDBColumnMetadata target) {
        return Integer.compare(sortIndex, target.getSortIndex());
    }

    @Override
    @SuppressWarnings("all")
    @SneakyThrows
    public RDBColumnMetadata clone() {
        return ((RDBColumnMetadata) super.clone());
    }

    @Override
    public String toString() {
        return "{" +
                "name='" + name + '\'' +
                ", alias='" + alias + '\'' +
                ", comment='" + comment + '\'' +
                ", dataType='" + dataType + '\'' +
                '}';
    }


    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.column;
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

    public String getFullName(String ownerName){
        return getDialect().buildColumnName(ownerName,getName());
    }

    public String getFullName(){
        return  getFullName(getOwner().getName());
    }

}
