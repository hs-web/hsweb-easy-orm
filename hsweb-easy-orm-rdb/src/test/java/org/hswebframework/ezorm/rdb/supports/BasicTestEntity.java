package org.hswebframework.ezorm.rdb.supports;

import lombok.*;
import org.hswebframework.ezorm.core.Extendable;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.annotation.DefaultValue;
import org.hswebframework.ezorm.rdb.mapping.annotation.EnumCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;

import javax.persistence.*;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.*;

@Table(name = "entity_test_table", indexes = @Index(name = "test_index", columnList = "name,state desc"))
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(exclude = {"createTime", "address"})
@ToString
public class BasicTestEntity implements Serializable, Extendable {

    @Column(length = 32)
    @Id
    @GeneratedValue(generator = "uuid")
    private String id;

    @Column(nullable = false)
    private String name;

    @Column(name = "create_time", updatable = false)
    private Date createTime;

    @Column(nullable = false)
    private Byte state;

    @Column
    private Double doubleVal;

    @Column
    private Float floatVal;


    @Column(scale = 1)
    private BigDecimal bigDecimal;

    @Column
//    @DefaultValue(generator = "random")
    private Long balance;

    @Column(table = "entity_test_table_detail")
    private String detail;

    @Column
    @ColumnType(jdbcType = JDBCType.LONGNVARCHAR)
    @JsonCodec
    private List<String> tags;

    @Column
    @ColumnType(javaType = String.class)
    @DefaultValue("disabled")
    private StateEnum stateEnum;

    @Column
    @ColumnType(javaType = Long.class, jdbcType = JDBCType.BIGINT)
    @EnumCodec(toMask = true)
    @DefaultValue("0")
    private StateEnum[] stateEnums;

    @Column
    private String aTest;

    @Column(name = "address_id")
    private String addressId;

    @Column(name = "bool_type")
    private Boolean enabled;


    @JoinColumn(name = "address_id")
    private Address address;

    @Builder.Default
    private transient Map<String, Object> extensions = new HashMap<>();

    @Override
    public Map<String, Object> extensions() {
        return extensions;
    }

    @Override
    public Object getExtension(String property) {
        return extensions.get(property);
    }

    @Override
    public void setExtension(String property, Object value) {
        extensions.put(property, value);
    }

}
