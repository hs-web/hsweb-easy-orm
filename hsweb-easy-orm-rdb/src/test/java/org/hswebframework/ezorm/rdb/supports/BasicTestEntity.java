package org.hswebframework.ezorm.rdb.supports;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

@Table(name = "entity_test_table")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BasicTestEntity implements Serializable {

    @Column(length = 32)
    @Id
    private String id;

    @Column
    private String name;

    @Column(name = "create_time")
    private Date createTime;

    @Column
    private Byte state;

    @Column
    private Long balance;

    @Column(table = "entity_test_table_detail")
    private String detail;


}
