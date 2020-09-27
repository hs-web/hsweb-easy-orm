package org.hswebframework.ezorm.core.param;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * @author zhouhao
 * @since 1.1
 */
@Getter
@Setter
@EqualsAndHashCode
public class Column implements Serializable {
    @Schema(description = "字段名")
    private String name;

    @Hidden
    private String type;
}
