package org.hswebframework.ezorm.rdb.supports;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.JDBCType;
import java.util.Date;
import java.util.List;

@Table(name = "entity_test_table", indexes = @Index(name = "test_index", columnList = "name,state desc"))
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BasicTestEntity implements Serializable {

    @Column(length = 32)
    @Id
    private String id;

    @Column(nullable = false)
    private String name;

    @Column(name = "create_time", updatable = false)
    private Date createTime;

    @Column(nullable = false)
    private Byte state;

    @Column
    private Long balance;

    @Column(table = "entity_test_table_detail")
    private String detail;

    @Column
    @ColumnType(jdbcType = JDBCType.CLOB)
    @JsonCodec
    private List<String> tags;

}
