package org.hswebframework.ezorm.core.param;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * 排序
 *
 * @author zhouhao
 * @since 1.0
 */
@EqualsAndHashCode(callSuper = true)
@Getter
@Setter
public class Sort extends Column {

    private String order = "asc";

    public Sort() {
    }
    public Sort(String column) {
        this.setName(column);
    }

    public String getOrder() {
        if ("desc".equalsIgnoreCase(order)) {
            return order;
        } else {
            return order = "asc";
        }
    }

    public void asc() {
        this.order = "asc";
    }

    public void desc() {
        this.order = "desc";
    }

}
