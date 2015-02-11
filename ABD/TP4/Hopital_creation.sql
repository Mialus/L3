CREATE TABLE MEDECIN
(
  MATRICULEMEDECIN  INT,
  NOM               VARCHAR(1000),
  PRENOM            VARCHAR(1000),
  CODESERVICE       INT,
  GRADE             VARCHAR(1000),
  DATEEMBAUCHE      DATE
);

CREATE TABLE MEDICAMENT
(
  CODEMEDICAMENT  INT,
  LIBELLE         VARCHAR(1000),
  FORME           VARCHAR(1000),
  PRIX            FLOAT
);

CREATE TABLE PATIENT
(
  NUMSECUPATIENT  INT,
  NOM             VARCHAR(1000),
  PRENOM          VARCHAR(1000),
  ADRESSE         VARCHAR(1000),
  RHESUS          VARCHAR(3)
);

CREATE TABLE PRESCRIPTION
(
  CODEVISITE      INT,
  CODEMEDICAMENT  INT,
  POSOLOGIE       INT,
  DUREE           INT
);

CREATE TABLE SERVICE
(
  CODESERVICE  INT,
  NOM          VARCHAR(1000),
  CATEGORIE    VARCHAR(1)
);

CREATE TABLE VISITE
(
  CODEVISITE        INT,
  MATRICULEMEDECIN  INT DEFAULT 5,
  NUMSECUPATIENT    INT,
  DATEETHEURE       DATE,
  DUREE             INT
);
