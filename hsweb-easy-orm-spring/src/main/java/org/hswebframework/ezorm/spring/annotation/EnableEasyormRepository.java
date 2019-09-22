package org.hswebframework.ezorm.spring.annotation;

import org.hswebframework.ezorm.spring.mapping.EasyormRepositoryScannerRegistrar;
import org.springframework.context.annotation.Import;

import java.lang.annotation.*;

/**
 *
 * @see org.hswebframework.ezorm.rdb.mapping.ReactiveRepository
 * @since 4.0.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
@Import(EasyormRepositoryScannerRegistrar.class)
public @interface EnableEasyormRepository {

    String[] value();

}
