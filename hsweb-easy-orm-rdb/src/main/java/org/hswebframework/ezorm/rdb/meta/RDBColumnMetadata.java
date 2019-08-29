package org.hswebframework.ezorm.rdb.meta;

import lombok.*;
import org.hswebframework.ezorm.core.meta.AbstractColumnMetadata;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.io.Serializable;
import java.sql.JDBCType;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RDBColumnMetadata extends AbstractColumnMetadata implements ColumnMetadata, Serializable, Cloneable, Comparable<RDBColumnMetadata> {

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
    private AbstractTableOrViewMetadata owner;

    @Override
    public int compareTo(RDBColumnMetadata o) {
        return Integer.compare(sortIndex, o.getSortIndex());
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
}
