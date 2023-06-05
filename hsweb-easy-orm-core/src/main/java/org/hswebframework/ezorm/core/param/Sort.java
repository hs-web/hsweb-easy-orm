package org.hswebframework.ezorm.core.param;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Map;

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

    @Schema(description = "排序方式", allowableValues = {"asc", "desc"}, minLength = 3, maxLength = 4)
    private String order = "asc";

    @Schema(description = "指定的值优先排序")
    private Object value;

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

    public Sort asc() {
        this.order = "asc";
        return this;
    }

    public Sort desc() {
        this.order = "desc";
        return this;
    }

    public Sort function(String function) {
        this.setType(function);
        return this;
    }

    public Sort options(Map<String, Object> options) {
        this.setOpts(options);
        return this;
    }

    public Sort option(String key, Object value) {
        super.option(key, value);
        return this;
    }

    public Sort value(Object value) {
        this.value = value;
        return this;
    }
}
