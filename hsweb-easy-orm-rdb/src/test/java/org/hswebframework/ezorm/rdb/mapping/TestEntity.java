package org.hswebframework.ezorm.rdb.mapping;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Getter
@Setter
@Table(name = "entity_test",
indexes = @Index(
        name = "test_index",
        columnList = "name asc,state desc"
))
@ToString
@EqualsAndHashCode
public class TestEntity implements Serializable {

    @Column
    @Id
    private String id;

    @Column
    private String name;

    @Column
    private Byte state;

    @Column(name = "create_time")
    private Date createTime;

    @Column(table = "entity_detail", name = "name")
//    @JoinColumn(table = "entity_detail", name = "id", referencedColumnName = "id")
    private String infoName;


}
