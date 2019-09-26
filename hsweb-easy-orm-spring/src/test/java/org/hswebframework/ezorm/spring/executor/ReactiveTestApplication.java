package org.hswebframework.ezorm.spring.executor;

import org.hswebframework.ezorm.spring.annotation.EnableEasyormRepository;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.transaction.TransactionAutoConfiguration;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@SpringBootApplication(exclude = TransactionAutoConfiguration.class
        ,  scanBasePackages = "org.hswebframework.ezorm.spring"
)
@EnableTransactionManagement
@EnableEasyormRepository(value = "org.hswebframework.ezorm.spring.mapping", enableReactive = true)
public class ReactiveTestApplication {

}
