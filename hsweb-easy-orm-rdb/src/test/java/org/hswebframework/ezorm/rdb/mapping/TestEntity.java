package org.hswebframework.ezorm.rdb.mapping;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@Table(name = "entity_test")
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
