package org.hswebframework.ezorm.spring.mapping;

import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.executor.ReactiveTestApplication;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import reactor.test.StepVerifier;


@SpringBootTest(classes = ReactiveTestApplication.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class EasyormRepositoryRegistrarTest {

    @Autowired
    DatabaseOperator operator;

    @Autowired
    private TransactionRepository repository;


    @Test
    public void test() {
        repository.test()
                .as(StepVerifier::create)
                .expectNext(3)
                .verifyComplete();
    }


}