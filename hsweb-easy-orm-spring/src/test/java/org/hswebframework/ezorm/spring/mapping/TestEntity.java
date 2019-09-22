package org.hswebframework.ezorm.spring.mapping;


import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;

@Data
@Table(name = "test_entity")
public class TestEntity {

    @Column
    @Id
    private String id;

    @Column
    private String name;
}
