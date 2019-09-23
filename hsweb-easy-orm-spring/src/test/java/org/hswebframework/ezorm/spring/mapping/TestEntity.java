package org.hswebframework.ezorm.spring.mapping;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;

@Data
@Table(name = "test_entity")
@Builder
@AllArgsConstructor(staticName = "of")
@NoArgsConstructor
public class TestEntity {

    @Column
    @Id
    private String id;

    @Column
    private String name;
}
